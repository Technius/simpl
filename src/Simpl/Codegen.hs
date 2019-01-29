{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- Suppress LLVM sum type of records AST warnings
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
module Simpl.Codegen where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable (para, unfix)
import Data.Text.Prettyprint.Doc (pretty)
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Map (Map)
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))

import Data.String (fromString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Linkage as LLVM
import qualified LLVM.AST.FloatingPointPredicate as LLVMFP
import qualified LLVM.AST.IntegerPredicate as LLVMIP
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

data CodegenTable =
  MkCodegenTable { tableVars :: Map Text LLVM.Operand -- ^ Pointer to variables
                 , tableCtors :: Map Text (LLVM.Name, LLVM.Name, Int) -- ^ Data type name, ctor name, index
                 , tableAdts :: Map Text (LLVM.Name, Type, [Constructor])
                 , tableFuns :: Map Text LLVM.Operand
                 , tableCurrentJoin :: LLVM.Name }
  deriving (Show)

-- | An empty codegen table. This will cause a crash if codegen is run when not
-- initialized!
emptyCodegenTable :: CodegenTable
emptyCodegenTable =
  MkCodegenTable { tableVars = Map.empty
                 , tableCtors = Map.empty
                 , tableAdts = Map.empty
                 , tableFuns = Map.empty
                 , tableCurrentJoin = LLVM.mkName "__default_join_point" }

newtype CodegenT m a =
  CodegenT { unCodegen :: StateT CodegenTable m a }
  deriving ( Functor
           , Applicative
           , MonadState CodegenTable
           , MonadFix)

type Codegen = CodegenT Identity

deriving instance Monad m => Monad (CodegenT m)

instance MonadTrans CodegenT where
  lift = CodegenT . lift

instance Monad m => MonadReader CodegenTable (CodegenT m) where
  ask = local id get
  local f action = do
    curTable <- get
    modify f
    result <- action
    put curTable
    pure result

data Result = Values (NonEmpty LLVM.Operand) | Branching (NonEmpty (LLVM.Operand, LLVM.Name))
  deriving (Show, Eq)

instance Ord Result where
  compare (Values v1) (Values v2) = compare v1 v2
  compare (Values _) (Branching _) = GT
  compare (Branching _) (Values _) = LT
  compare (Branching br1) (Branching br2) = compare br1 br2

resultHasJump :: Result -> Bool
resultHasJump (Values _) = False
resultHasJump (Branching _) = True

joinPoint :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
          => m (NonEmpty Result)
          -> m (NonEmpty LLVM.Operand)
joinPoint resultsM = do
  currentJp <- gets tableCurrentJoin
  newJp <- LLVMIR.freshName "join_point"
  modify (\t -> t { tableCurrentJoin = newJp })
  results <- resultsM
    -- modify (\t -> t { tableCurrentJoin = newJp })
  modify (\t -> t { tableCurrentJoin = currentJp })
  let hasJump = any resultHasJump results
  when hasJump $
    LLVMIR.emitBlockStart newJp
  let go (Values v) = pure v
      go (Branching brs) =
        if length brs == 1
          then pure (fst <$> brs)
          else (:| []) <$> LLVMIR.phi (NE.toList brs)
  join <$> traverse go (NE.sort results)

joinPoint1 :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
           => m Result
           -> m LLVM.Operand
joinPoint1 = fmap NE.head . joinPoint . fmap (:| [])

resultValue :: LLVM.Operand -> Result
resultValue v = Values (v :| [])

resultBranching :: NonEmpty (LLVM.Operand, LLVM.Name) -> Result
resultBranching = Branching

resultEnsureBranch :: LLVMIR.MonadIRBuilder m => Result -> m Result
resultEnsureBranch = \case
  Values vs -> LLVMIR.currentBlock >>= \cb -> pure $ Branching ((, cb) <$> vs)
  b@Branching {} -> pure b

resultCombine :: LLVMIR.MonadIRBuilder m => Result -> Result -> m Result
resultCombine (Values v1) (Values v2) = pure $ Values (v1 <> v2)
resultCombine (Values v1) (Branching br2) =
  LLVMIR.currentBlock >>= \cb -> pure $ Branching (((, cb) <$> v1) <> br2)
resultCombine (Branching br1) (Values v2) =
  LLVMIR.currentBlock >>= \cb -> pure $ Branching (br1 <> ((, cb) <$> v2))
resultCombine (Branching br1) (Branching br2) = pure $ Branching (br1 <> br2)

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

literalToLLVM :: LLVMIR.MonadIRBuilder m => Literal -> m Result
literalToLLVM = \case
  LitInt x -> resultValue <$> LLVMIR.int64 (fromIntegral x)
  LitDouble x -> resultValue <$> LLVMIR.double x
  LitBool b -> resultValue <$> LLVMIR.bit (if b then 1 else 0)

-- | Generates code for a binary operation
binopCodegen :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
      => m Result
      -> m Result
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand)
      -> m Result
binopCodegen x y op = do
  x' <- joinPoint1 x
  y' <- joinPoint1 y
  res <- op x' y'
  pure $ resultValue res

-- | Generates code for a numeric binary operation
numBinopCodegen :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
      => m Result
      -> m Result
      -> Type
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand) -- ^ Float operation
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand) -- ^ Integer operation
      -> m Result
numBinopCodegen x y ty opDouble opInt =
  case unfix ty of
    TyNumber numTy ->
      if numTy == NumInt then binopCodegen x y opInt
                         else binopCodegen x y opDouble
    _ -> error "Invariant violated"

binopToLLVM :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
            => BinaryOp -- ^ Operation
            -> Type -- ^ Type of the inputs
            -> m Result
            -> m Result
            -> m Result
binopToLLVM op ty x y =
  let (floatInstr, intInstr) = case op of
        Add -> (LLVMIR.fadd, LLVMIR.add)
        Sub -> (LLVMIR.fsub, LLVMIR.sub)
        Mul -> (LLVMIR.fmul, LLVMIR.mul)
        Div -> (LLVMIR.fdiv, LLVMIR.sdiv)
        Lt -> ((LLVMIR.fcmp LLVMFP.OLT), (LLVMIR.icmp LLVMIP.SLT))
        Lte -> ((LLVMIR.fcmp LLVMFP.OLE), (LLVMIR.icmp LLVMIP.SLE))
        Equal -> ((LLVMIR.fcmp LLVMFP.OEQ), (LLVMIR.icmp LLVMIP.EQ))
  in numBinopCodegen x y ty floatInstr intInstr

arithToLLVM :: TypedExpr -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) Result
arithToLLVM = para (go . annGetExpr)
  where
    go :: ExprF (TypedExpr, LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) Result)
       -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) Result
    go = \case
      Lit l -> literalToLLVM l
      BinOp op (ex, x) (_, y) -> binopToLLVM op (annGetAnn (unfix ex)) x y
      If (_, condInstr) (_, t1Instr) (_, t2Instr) -> do
        LLVMIR.ensureBlock
        cond <- joinPoint1 condInstr
        trueLabel <- LLVMIR.freshName "if_then"
        falseLabel <- LLVMIR.freshName "if_else"
        LLVMIR.condBr cond trueLabel falseLabel
        LLVMIR.emitBlockStart trueLabel
        t1Res <- t1Instr >>= resultEnsureBranch
        jp <- gets tableCurrentJoin
        LLVMIR.br jp
        LLVMIR.emitBlockStart falseLabel
        t2Res <- t2Instr >>= resultEnsureBranch
        LLVMIR.br jp
        resultCombine t1Res t2Res
      Cons name argPairs -> do
        let args = snd <$> argPairs
        -- Assume constructor exists, since typechecker should verify it anyways
        (dataTy, ctorName, ctorIndex) <- asks (fromJust . Map.lookup name . tableCtors)
        ctorIndex' <- LLVMIR.int32 (fromIntegral ctorIndex)
        let tagStruct1 = LLVM.ConstantOperand $ LLVMC.Undef (LLVM.NamedTypeReference dataTy)
        -- Tag
        tagStruct2 <- LLVMIR.insertValue tagStruct1 ctorIndex' [0]
        -- Data pointer
        let ctorTy = LLVM.NamedTypeReference ctorName
        let nullptr = LLVM.ConstantOperand (LLVMC.Null (LLVM.ptr ctorTy))
        -- Use offsets to calculate struct size
        ctorStructSize <- flip LLVMIR.ptrtoint LLVM.i64
                          =<< LLVMIR.gep nullptr
                          =<< pure <$> LLVMIR.int32 0
        -- Allocate memory for constructor.
        -- For now, use "leak memory" as an implementation strategy for deallocation.
        ctorStructPtr <- LLVMIR.call mallocRef [(ctorStructSize, [])] >>=
                         flip LLVMIR.bitcast (LLVM.ptr ctorTy)
        if null args
          then pure ()
          else do
            values <- joinPoint (NE.fromList <$> sequence args)
            indices <- traverse LLVMIR.int32 [0..fromIntegral (length values - 1)]
            ptrOffset <- LLVMIR.int32 0
            forM_ (indices `zip` NE.toList values) $ \(index, v) -> do
              valuePtr <- LLVMIR.gep ctorStructPtr [ptrOffset, index]
              LLVMIR.store valuePtr 0 v
              pure ()
        dataPtr <- LLVMIR.bitcast ctorStructPtr (LLVM.ptr LLVM.i8)
        fullyInit <- LLVMIR.insertValue tagStruct2 dataPtr [1]
        pure $ resultValue fullyInit
      Case branches (valExpr, valM) -> do
        LLVMIR.ensureBlock
        defLabel <- LLVMIR.freshName "case_default"
        endLabel <- gets tableCurrentJoin
        val <- joinPoint1 valM
        allCaseLabels <- forM branches $ \case
          BrAdt name _ _ ->
            let labelName = "case_" <> fromString (Text.unpack name) in
            (name, ) <$> LLVMIR.freshName labelName
        -- Assume the symbol table and type information is correct
        let dataName = fromJust $
              case unfix . annGetAnn . unfix $ valExpr of { TyAdt n -> Just n; _ -> Nothing }
        ctors <- asks ((\(_,_,cs) -> cs) . fromJust . Map.lookup dataName . tableAdts)
        let ctorNames = ctorGetName <$> ctors
        let usedLabelTriples = filter ((`elem` ctorNames) . fst . snd) $ [0..] `zip` allCaseLabels
        let jumpTable = (\(i, (_, l)) -> (LLVMC.Int 32 i, l)) <$> usedLabelTriples
        tag <- LLVMIR.extractValue val [0]
        dataPtr <- LLVMIR.extractValue val [1]
        LLVMIR.switch tag defLabel jumpTable
        resVals <- forM (usedLabelTriples `zip` branches) $ \((_, (ctorName, label)), br) -> do
          let expr = snd (branchGetExpr br)
          (_, ctorLLVMName, index) <- asks (fromJust . Map.lookup ctorName . tableCtors)
          let Ctor _ argTys = ctors !! index
          let bindingPairs = branchGetBindings br `zip` (typeToLLVM <$> argTys)
          LLVMIR.emitBlockStart label
          ctorPtr <- LLVMIR.bitcast dataPtr (LLVM.ptr (LLVM.NamedTypeReference ctorLLVMName))
          ctorPtrOffset <- LLVMIR.int32 0
          bindings <- forM ([0..] `zip` bindingPairs) $ \(i, (n, ty)) -> do
            ctorPtrIndex <- LLVMIR.int32 i
            -- Need to bitcast the ptr type because we need a concrete type. We
            -- also need to load the data immediately because of how variables
            -- are implemented.
            v <- LLVMIR.gep ctorPtr [ctorPtrOffset, ctorPtrIndex]
                 >>= flip LLVMIR.bitcast (LLVM.ptr ty)
                 >>= flip LLVMIR.load 0
            pure (n, v)
          let updateTable t = t { tableVars = Map.union (tableVars t) (Map.fromList bindings) }
          res <- local updateTable expr >>= resultEnsureBranch
          LLVMIR.br endLabel
          pure res
        LLVMIR.emitBlockStart defLabel
        LLVMIR.unreachable
        foldM resultCombine (head resVals) (tail resVals)
        -- resultCombine pure (sconcat (NE.fromList resVals))
      Let name (_, valM) (_, exprM) -> do
        val <- joinPoint1 valM
        local (\t -> t { tableVars = Map.insert name val (tableVars t) }) exprM
      Var name -> do
        -- Assume codegen table is correct
        value <- gets (fromJust . Map.lookup name . tableVars)
        pure $ resultValue value
      App name argsM -> do
        args <- traverse joinPoint1 (snd <$> argsM)
        -- Assume that name is callable (e.g. either a static function or a
        -- function pointer)
        fn <- fromJust <$> lookupName name
        resultValue <$> LLVMIR.call fn ((, []) <$> args)
      FunRef name -> do
        fn <- gets (fromJust . Map.lookup name . tableFuns)
        pure (resultValue fn)
      Cast (origExpr, exprM) num -> do
        let origNum = case unfix (annGetAnn (unfix origExpr)) of
              TyNumber n -> n
              _ -> error "codegen: attempting to cast non-numeric"
        expr <- joinPoint1 exprM
        resultValue <$> castOp origNum num expr
    lookupName :: MonadState CodegenTable m
               => Text
               -> m (Maybe LLVM.Operand)
    lookupName name =
      gets $ \t ->
        Map.lookup name (tableVars t) <|> Map.lookup name (tableFuns t)
    castOp :: LLVMIR.MonadIRBuilder m => Numeric -> Numeric -> LLVM.Operand -> m LLVM.Operand
    castOp source castTo oper = case (source, castTo) of
      (NumDouble, NumDouble) -> pure oper
      (NumInt, NumInt) -> pure oper
      (NumDouble, NumInt) -> LLVMIR.fptosi oper LLVM.i64
      (NumInt, NumDouble) -> LLVMIR.sitofp oper LLVM.double
      (NumUnknown, _) -> castOp NumDouble castTo oper
      (_, NumUnknown) -> error "castOp: attempting to cast to NumUnknown"

ctorToLLVM :: Constructor -> [LLVM.Type]
ctorToLLVM (Ctor _ args) = typeToLLVM <$> args

typeToLLVM :: Type -> LLVM.Type
typeToLLVM = go . unfix
  where
    go = \case
      TyNumber n -> case n of
        NumDouble -> LLVM.double
        NumInt -> LLVM.i64
        NumUnknown -> LLVM.double
      TyBool -> LLVM.i1
      TyAdt name -> LLVM.NamedTypeReference (llvmName name)
      TyFun args res ->
        LLVM.ptr $ LLVM.FunctionType
            { LLVM.resultType = typeToLLVM res
            , LLVM.argumentTypes = typeToLLVM <$> args
            , LLVM.isVarArg = False
            }

adtToLLVM :: Text
           -> [Constructor]
           -> LLVMIR.ModuleBuilderT Codegen ()
adtToLLVM adtName ctors = do
  let adtType = LLVM.StructureType
        { LLVM.isPacked = True
        , LLVM.elementTypes = [LLVM.i32, LLVM.ptr LLVM.i8] }
  adtLLVMName <- gets ((\(n,_,_) -> n) . fromJust . Map.lookup adtName . tableAdts)
  -- TODO: Store returned type in symbol table to avoid error-prone type
  -- reconstruction
  _ <- LLVMIR.typedef adtLLVMName (Just adtType)
  forM_ ctors $ \(Ctor ctorName args) -> do
    let ctorType = LLVM.StructureType
          { LLVM.isPacked = True
          , LLVM.elementTypes = typeToLLVM <$> args }
    ctorLLVMName <- gets ((\(_,n,_) -> n) . fromJust . Map.lookup ctorName . tableCtors)
    LLVMIR.typedef ctorLLVMName (Just ctorType)

-- | Emits the given function definition
funToLLVM :: Text
           -> [(Text, Type)]
           -> Type
           -> TypedExpr
           -> LLVMIR.ModuleBuilderT Codegen LLVM.Operand
funToLLVM name params ty body =
    let name' = if name == "main" then "__simpl_main" else name
        ftype = typeToLLVM ty
        fname = llvmName name'
        fparams = [(typeToLLVM t, fromString (Text.unpack n)) | (n, t) <- params]
    in mdo foper <- LLVMIR.function fname fparams ftype $ \args -> do
             LLVMIR.ensureBlock
             endLabel <- LLVMIR.freshName "function_end"
             -- We need to make sure we don't pollute other function scopes
             oldVars <- gets tableVars
             let updVars t = tableVars t `Map.union` Map.fromList ((fst <$> params) `zip` args)
             modify (\t -> t { tableCurrentJoin = endLabel
                             , tableFuns = Map.insert name foper (tableFuns t)
                             , tableVars = updVars t })
             retval <- joinPoint1 (arithToLLVM body)
             -- Restore old scope
             modify (\t -> t { tableVars = oldVars })
             LLVMIR.ret retval
           pure foper

initCodegenTable :: SymbolTable TypedExpr -> Codegen ()
initCodegenTable symTab = do
  let adts = flip Map.mapWithKey (symTabAdts symTab) $ \name (ty, ctors) -> (llvmName name, ty, ctors)
  ctors <- forM (Map.elems adts) $ \(adtName, _, ctors) ->
    forM ([0..] `zip` ctors) $ \(i, Ctor ctorName _) ->
      pure (ctorName, (adtName, llvmName ctorName, i))
  modify $ \t -> t { tableAdts = adts
                   , tableCtors = Map.fromList (join ctors) }

generateLLVM :: [Decl Expr]
             -> SymbolTable TypedExpr
             -> LLVMIR.ModuleBuilderT Codegen ()
generateLLVM decls symTab = mdo
  -- Message is "Hi\n" (with null terminator)
  (_, msg) <- staticString ".message" "Hello world!\n"
  (_, resultFmt) <- staticString ".resultformat" "Result: %.i\n"
  let srcCode = unlines $ show . pretty <$> decls
  (_, exprSrc) <- staticString ".sourcecode" $ "Source code: " ++ srcCode ++ "\n"
  printf <- llvmPrintf
  _ <- llvmEmitMalloc
  forM_ (Map.toList . symTabAdts $ symTab) $ \(name, (_, ctors)) ->
    adtToLLVM name ctors
  -- Insert function operands into symbol table before emitting so order of
  -- definition doesn't matter. This works because the codegen monad is lazy.
  modify (\t -> t { tableFuns = tableFuns t `Map.union` Map.fromList funOpers })
  funOpers <- forM (Map.toList . symTabFuns $ symTab) $ \(name, (params, ty, body)) ->
    (name, ) <$> funToLLVM name params ty body

  _ <- LLVMIR.function "main" [] LLVM.i64 $ \_ -> do
    _ <- LLVMIR.call printf [(msg, [])]
    let mainTy = LLVM.ptr (LLVM.FunctionType LLVM.i64 [] False)
    let mainName = LLVM.mkName "__simpl_main"
    let mainRef = LLVM.ConstantOperand (LLVMC.GlobalReference mainTy mainName)
    exprResult <- LLVMIR.call mainRef []
    _ <- LLVMIR.call printf [(exprSrc, [])]
    _ <- LLVMIR.call printf [(resultFmt, []), (exprResult, [])]
    retcode <- LLVMIR.int64 1
    LLVMIR.ret retcode
  pure ()

runCodegen :: [Decl Expr] -> SymbolTable TypedExpr -> LLVM.Module
runCodegen decls symTab
  = runIdentity
  . flip evalStateT emptyCodegenTable
  . unCodegen
  . LLVMIR.buildModuleT "simpl.ll"
  $ lift (initCodegenTable symTab) >> generateLLVM decls symTab
