{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- Suppress LLVM sum type of records AST warnings
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Simpl.Codegen where

import Control.Monad (liftM2)
import Data.Functor.Foldable (cata)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Char (ord)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Linkage as LLVM
import qualified LLVM.AST.Constant as LLVMC
import qualified LLVM.AST.Global as LLVMG
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Module as LLVMIR
import qualified LLVM.IRBuilder.Monad as LLVMIR
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Constant as LLVMIR

import Simpl.Ast

llvmByte :: Integer -> LLVMC.Constant
llvmByte = LLVMC.Int 8

llvmString :: String -> (Int, LLVMC.Constant)
llvmString s =
  let s' = s ++ "\0"
      arr = LLVMC.Array LLVM.i8 (fmap (llvmByte . toInteger . ord) s')
  in (length s', arr)

staticString :: LLVMIR.MonadModuleBuilder m => LLVM.Name -> String -> m (Int, LLVM.Operand)
staticString name str = do
  let (messageLen, bytes) = llvmString str
      messageTy = LLVM.ArrayType { LLVM.nArrayElements = toEnum messageLen
                                 , LLVM.elementType = LLVM.i8 }
  LLVMIR.emitDefn (LLVM.GlobalDefinition $ LLVM.globalVariableDefaults
                  { LLVMG.name = name
                  , LLVMG.isConstant = True
                  , LLVMG.unnamedAddr = Just LLVMG.GlobalAddr
                  , LLVMG.type' = messageTy
                  , LLVMG.linkage = LLVM.Private
                  , LLVMG.initializer = Just bytes
                  , LLVMG.alignment = 1
                  })
  let msgPtrTy = LLVM.ptr messageTy
      msgPtr = LLVM.ConstantOperand $
        LLVMC.GetElementPtr
        { LLVMC.inBounds = True
        , LLVMC.address = LLVMC.GlobalReference msgPtrTy name
        , LLVMC.indices = [LLVMC.Int 32 0, LLVMC.Int 32 0] }
  pure (messageLen, msgPtr)

llvmPrintf :: LLVMIR.MonadModuleBuilder m => m LLVM.Operand
llvmPrintf = do
  let argTy = LLVM.ptr LLVM.i8
  let printfTy = LLVM.ptr $ LLVM.FunctionType LLVM.void [argTy] True
  LLVMIR.emitDefn $ LLVM.GlobalDefinition LLVMG.functionDefaults
    { LLVMG.name = "printf"
    , LLVMG.linkage = LLVM.External
    , LLVMG.parameters = ([LLVM.Parameter argTy "" []], True)
    , LLVMG.returnType = LLVM.void
    }
  pure $ LLVM.ConstantOperand (LLVMC.GlobalReference printfTy "printf")

literalToLLVM :: LLVMIR.MonadIRBuilder m => Literal -> m LLVM.Operand
literalToLLVM = \case
  LitDouble x -> LLVMIR.double x
  LitBool b -> LLVMIR.bit (if b then 1 else 0)

arithToLLVM :: LLVMIR.MonadIRBuilder m => Expr -> m LLVM.Operand
arithToLLVM = cata $ \case
  Lit l -> literalToLLVM l
  Add x y -> liftM2 (,) x y >>= uncurry LLVMIR.fadd
  Sub x y -> liftM2 (,) x y >>= uncurry LLVMIR.fsub
  Mul x y -> liftM2 (,) x y >>= uncurry LLVMIR.fmul
  Div x y -> liftM2 (,) x y >>= uncurry LLVMIR.fdiv
  If condInstr t1Instr t2Instr -> do
    LLVMIR.ensureBlock
    cond <- condInstr
    trueLabel <- LLVMIR.freshName "if_then"
    falseLabel <- LLVMIR.freshName "if_else"
    endLabel <- LLVMIR.freshName "if_end"
    LLVMIR.condBr cond trueLabel falseLabel
    LLVMIR.emitBlockStart trueLabel
    t1Res <- t1Instr
    LLVMIR.br endLabel
    LLVMIR.emitBlockStart falseLabel
    t2Res <- t2Instr
    LLVMIR.br endLabel
    LLVMIR.emitBlockStart endLabel
    LLVMIR.phi [(t1Res, trueLabel), (t2Res, falseLabel)]

generateLLVM :: Expr -> LLVM.Module
generateLLVM expr = LLVMIR.buildModule "simpl.ll" $ do
  -- Message is "Hi\n" (with null terminator)
  (_, msg) <- staticString ".message" "Hello world!\n"
  (_, resultFmt) <- staticString ".resultformat" "Result: %.f\n"
  (_, exprSrc) <- staticString ".sourcecode" $ "Source code: " ++ show (pretty expr) ++ "\n"
  printf <- llvmPrintf
  _ <- LLVMIR.function "main" [] LLVM.i64 $ \_ -> do
    _ <- LLVMIR.call printf [(msg, [])]

    exprResult <- arithToLLVM expr
    _ <- LLVMIR.call printf [(exprSrc, [])]
    _ <- LLVMIR.call printf [(resultFmt, []), (exprResult, [])]
    retcode <- LLVMIR.int64 1
    LLVMIR.ret retcode
  pure ()
