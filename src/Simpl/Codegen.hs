{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- Suppress LLVM sum type of records AST warnings
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simpl.Codegen where

import Control.Monad (liftM2, forM, forM_, mapM_)
import Control.Monad.Reader
import Data.Functor.Foldable (para, unfix)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
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
import Simpl.Analysis
import Simpl.Typing (TypedExpr)

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

llvmName :: Text -> LLVM.Name
llvmName = LLVM.mkName . Text.unpack

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

llvmEmitMalloc :: LLVMIR.MonadModuleBuilder m => m LLVM.Operand
llvmEmitMalloc =
  LLVMIR.extern (LLVM.mkName "malloc") [LLVM.i64] (LLVM.ptr LLVM.i8)

mallocRef :: LLVM.Operand
mallocRef = LLVM.ConstantOperand $
  LLVMC.GlobalReference
  (LLVM.ptr $ LLVM.FunctionType { LLVM.resultType = LLVM.ptr LLVM.i8
                                , LLVM.argumentTypes = [LLVM.i64]
                                , LLVM.isVarArg = False })
  (LLVM.mkName "malloc")

literalToLLVM :: LLVMIR.MonadIRBuilder m => Literal -> m LLVM.Operand
literalToLLVM = \case
  LitDouble x -> LLVMIR.double x
  LitBool b -> LLVMIR.bit (if b then 1 else 0)

arithToLLVM :: forall m. (LLVMIR.MonadIRBuilder m, MonadReader (SymbolTable TypedExpr) m)
            => TypedExpr
            -> m LLVM.Operand
arithToLLVM = para (\typedExpr -> go (annGetExpr typedExpr))
  where
    go :: ExprF (TypedExpr, m LLVM.Operand) -> m LLVM.Operand
    go = \case
      Lit l -> literalToLLVM l
      Add (_, x) (_, y) -> liftM2 (,) x y >>= uncurry LLVMIR.fadd
      Sub (_, x) (_, y) -> liftM2 (,) x y >>= uncurry LLVMIR.fsub
      Mul (_, x) (_, y) -> liftM2 (,) x y >>= uncurry LLVMIR.fmul
      Div (_, x) (_, y) -> liftM2 (,) x y >>= uncurry LLVMIR.fdiv
      If (_, condInstr) (_, t1Instr) (_, t2Instr) -> do
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
      Cons ctorName argPairs -> do
        let args = snd <$> argPairs
        -- Assume constructor exists, since typechecker should verify it anyways
        (dataTy, _, ctorIndex) <- asks (fromJust . symTabLookupCtor ctorName)
        ctorIndex' <- LLVMIR.int32 (fromIntegral ctorIndex)
        let tagStruct1 = LLVM.ConstantOperand $ LLVMC.Undef (typeToLLVM dataTy)
        -- Tag
        tagStruct2 <- LLVMIR.insertValue tagStruct1 ctorIndex' [0]
        -- Data pointer
        let ctorTy = LLVM.NamedTypeReference (llvmName ctorName)
        let nullptr = LLVM.ConstantOperand (LLVMC.Null (LLVM.ptr ctorTy))
        -- Use offsets to calculate struct size
        ctorStructSize <- flip LLVMIR.ptrtoint LLVM.i64
                          =<< LLVMIR.gep nullptr
                          =<< pure <$> LLVMIR.int32 0
        -- Allocate memory for constructor.
        -- For now, use "leak memory" as an implementation strategy for deallocation.
        ctorStructPtr <- LLVMIR.call mallocRef [(ctorStructSize, [])] >>=
                         flip LLVMIR.bitcast (LLVM.ptr ctorTy)
        values <- sequence args
        indices <- traverse LLVMIR.int32 [0..fromIntegral (length values - 1)]
        ptrOffset <- LLVMIR.int32 0
        forM_ (indices `zip` values) $ \(index, v) -> do
          valuePtr <- LLVMIR.gep ctorStructPtr [ptrOffset, index]
          LLVMIR.store valuePtr 0 v
          pure ()
        dataPtr <- LLVMIR.bitcast ctorStructPtr (LLVM.ptr LLVM.i8)
        LLVMIR.insertValue tagStruct2 dataPtr [1]
      Case branches (valExpr, valM) -> do
        LLVMIR.ensureBlock
        allCaseLabels <- forM branches $ \case
          BrAdt name _ _ -> (name, ) <$> LLVMIR.fresh
        defLabel <- LLVMIR.freshName "case_default"
        endLabel <- LLVMIR.freshName "case_end"
        val <- valM
        -- Assume the symbol table and type information is correct
        let dataName = fromJust $
              case unfix . annGetAnn . unfix $ valExpr of { TyAdt n -> Just n; _ -> Nothing }
        ctorNames <- asks (fmap ctorGetName . snd . fromJust . symTabLookupAdt dataName)
        let usedLabelTriples = filter ((`elem` ctorNames) . fst . snd) $ [0..] `zip` allCaseLabels
        let jumpTable = (\(i, (_, l)) -> (LLVMC.Int 32 i, l)) <$> usedLabelTriples
        let caseLabels = snd <$> jumpTable
        tag <- LLVMIR.extractValue val [0]
        LLVMIR.switch tag defLabel jumpTable
        resVals <- forM (jumpTable `zip` (snd . branchGetExpr <$> branches)) $ \((_, label), expr) -> do
          LLVMIR.emitBlockStart label
          res <- expr
          LLVMIR.br endLabel
          pure res
        LLVMIR.emitBlockStart defLabel
        LLVMIR.unreachable
        LLVMIR.emitBlockStart endLabel
        LLVMIR.phi (resVals `zip` caseLabels)

ctorToLLVM :: Constructor -> [LLVM.Type]
ctorToLLVM (Ctor _ args) = typeToLLVM <$> args

typeToLLVM :: Type -> LLVM.Type
typeToLLVM = go . unfix
  where
    go = \case
      TyDouble -> LLVM.double
      TyBool -> LLVM.i1
      TyAdt name -> LLVM.NamedTypeReference (llvmName name)

adtToLLVM :: LLVMIR.MonadModuleBuilder m
           => Text
           -> [Constructor]
           -> m ()
adtToLLVM adtName ctors = do
  let adtType = LLVM.StructureType
        { LLVM.isPacked = True
        , LLVM.elementTypes = [LLVM.i32, LLVM.ptr LLVM.i8] }
  LLVMIR.typedef (llvmName adtName) (Just adtType)
  forM_ ctors $ \(Ctor ctorName args) -> do
    let ctorType = LLVM.StructureType
          { LLVM.isPacked = True
          , LLVM.elementTypes = typeToLLVM <$> args }
    LLVMIR.typedef (llvmName ctorName) (Just ctorType)

funToLLVM :: (LLVMIR.MonadModuleBuilder m, MonadReader (SymbolTable TypedExpr) m)
           => Text
           -> Type
           -> TypedExpr
           -> m LLVM.Operand
funToLLVM name ty body =
    let name' = if name == "main" then "__simpl_main" else name
        defn = LLVMIR.function (llvmName name') [] (typeToLLVM ty) $ \_args -> do
          retval <- arithToLLVM body
          LLVMIR.ret retval
    in defn

generateLLVM :: [Decl Expr] -> Reader (SymbolTable TypedExpr) LLVM.Module
generateLLVM decls = LLVMIR.buildModuleT "simpl.ll" $ do
  -- Message is "Hi\n" (with null terminator)
  (_, msg) <- staticString ".message" "Hello world!\n"
  (_, resultFmt) <- staticString ".resultformat" "Result: %.f\n"
  let srcCode = unlines $ show . pretty <$> decls
  (_, exprSrc) <- staticString ".sourcecode" $ "Source code: " ++ srcCode ++ "\n"
  printf <- llvmPrintf
  _ <- llvmEmitMalloc
  asks (Map.toList . symTabFuns) >>= (mapM_ $ \(name, (ty, body)) -> funToLLVM name ty body)
  asks (Map.toList . symTabAdts) >>= (mapM_ $ \(name, (_, ctors)) -> adtToLLVM name ctors)

  _ <- LLVMIR.function "main" [] LLVM.i64 $ \_ -> do
    _ <- LLVMIR.call printf [(msg, [])]
    let mainTy = LLVM.ptr (LLVM.FunctionType LLVM.double [] False)
    let mainName = LLVM.mkName "__simpl_main"
    let mainRef = LLVM.ConstantOperand (LLVMC.GlobalReference mainTy mainName)
    exprResult <- LLVMIR.call mainRef []
    _ <- LLVMIR.call printf [(exprSrc, [])]
    _ <- LLVMIR.call printf [(resultFmt, []), (exprResult, [])]
    retcode <- LLVMIR.int64 1
    LLVMIR.ret retcode
  pure ()
