{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
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
module Simpl.Backend.Codegen where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable (unfix, Fix(..))
import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString ()
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Functor.Identity

import Data.String (fromString)
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

import Simpl.Annotation hiding (AnnExpr, AnnExprF)
import Simpl.Ast (BinaryOp(..), Constructor(..), Literal(..))
import Simpl.CompilerOptions
import Simpl.SymbolTable
import Simpl.Type (Type, TypeF(..), Numeric(..))
import Simpl.Typecheck (literalType)
import Simpl.Backend.Runtime ()
import Simpl.JoinIR.Syntax
import qualified Simpl.Backend.Runtime as RT

data CodegenTable =
  MkCodegenTable { tableVars :: Map Text (TypeF Type, LLVM.Operand) -- ^ Pointer to variables
                 , tableCtors :: Map Text (LLVM.Name, LLVM.Name, Int) -- ^ Data type name, ctor name, index
                 , tableAdts :: Map Text (LLVM.Name, Type, [Constructor])
                 , tableFuns :: Map Text LLVM.Operand
                 , tableJoinValues :: Map Text (LLVM.Name, [(LLVM.Operand, LLVM.Name)])
                 , tablePrintf :: LLVM.Operand
                 , tableOptions :: CompilerOpts }
  deriving (Show)

-- | An empty codegen table. This will cause a crash if codegen is run when not
-- initialized!
emptyCodegenTable :: CodegenTable
emptyCodegenTable =
  MkCodegenTable { tableVars = Map.empty
                 , tableCtors = Map.empty
                 , tableAdts = Map.empty
                 , tableFuns = Map.empty
                 , tableJoinValues = Map.empty
                 , tablePrintf = error "printf not set"
                 , tableOptions = defaultCompilerOpts }

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

localCodegenTable :: MonadState CodegenTable m
                  => (CodegenTable -> CodegenTable)
                  -> m a
                  -> m a
localCodegenTable f ma = do
  oldTable <- get
  put (f oldTable)
  res <- ma
  modify $ \t -> t
    { tableVars = tableVars oldTable
    , tableFuns = tableFuns oldTable
    }
  pure res

lookupName :: MonadState CodegenTable m
           => Text
           -> m (Maybe LLVM.Operand)
lookupName name =
  gets $ \t ->
    (snd <$> Map.lookup name (tableVars t)) <|> Map.lookup name (tableFuns t)

-- | Looks up the type of the [JValue]. Note: assumes that if the [JValue] is a
-- [JVar], then the variable is actually in the table.
lookupValueType :: MonadState CodegenTable m
              => JValue
              -> m (TypeF Type)
lookupValueType = \case
  JVar name -> gets (fst . fromJust . Map.lookup name . tableVars)
  JLit lit -> pure $ literalType lit

bindVariable :: MonadState CodegenTable m
             => Text
             -> TypeF Type
             -> LLVM.Operand
             -> m ()
bindVariable name ty oper =
  modify (\t -> t { tableVars = Map.insert name (ty, oper) (tableVars t) })

initCodegenTable :: CompilerOpts -> SymbolTable (AnnExpr fields) -> Codegen ()
initCodegenTable options symTab = do
  let adts = flip Map.mapWithKey (symTabAdts symTab) $ \name (ty, ctors) -> (llvmName name, ty, ctors)
  ctors <- forM (Map.elems adts) $ \(adtName, _, ctors) ->
    forM ([0..] `zip` ctors) $ \(i, Ctor ctorName _) ->
      pure (ctorName, (adtName, llvmName ctorName, i))
  modify $ \t -> t { tableAdts = adts
                   , tableCtors = Map.fromList (join ctors)
                   , tableOptions = options }

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

-- | Generates LLVM code for a literal.
literalCodegen :: LLVMIR.MonadIRBuilder m => Literal -> m LLVM.Operand
literalCodegen = \case
  LitInt x -> LLVMIR.int64 (fromIntegral x)
  LitDouble x -> LLVMIR.double x
  LitBool b -> LLVMIR.bit (if b then 1 else 0)
  LitString t -> do
    -- TODO: Store literal strings in global memory
    let byteS = encodeUtf8 t
    let len = toInteger (BS.length byteS)
    lenOper <- LLVMIR.int64 len
    let byteData = LLVMC.Int 8 . toInteger <$> BS.unpack byteS
    byteDataOper <- LLVMIR.array byteData
    byteDataPtr <- LLVMIR.alloca (LLVM.ArrayType (fromInteger len) LLVM.i8) Nothing 0
    _ <- LLVMIR.store byteDataPtr 0 byteDataOper
    byteDataPtr' <- LLVMIR.bitcast byteDataPtr (LLVM.ptr LLVM.i8)
    bytePtr <- LLVMIR.call RT.mallocRef [(lenOper, [])]
    _ <- LLVMIR.call RT.memcpyRef [(bytePtr, []), (byteDataPtr', []), (lenOper, [])]
    LLVMIR.call RT.stringNewRef [(lenOper, []), (bytePtr, [])]

-- | Generates code for an arbitrary binary operation
binaryOpCodegen
      :: LLVMIR.MonadIRBuilder m
      => m LLVM.Operand
      -> m LLVM.Operand
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand)
      -> m LLVM.Operand
binaryOpCodegen x y op = do
  x' <- x
  y' <- y
  op x' y'

-- | Generates code for a numeric binary operation
numBinopCodegen :: LLVMIR.MonadIRBuilder m
      => m LLVM.Operand
      -> m LLVM.Operand
      -> Type
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand) -- ^ Float operation
      -> (LLVM.Operand -> LLVM.Operand -> m LLVM.Operand) -- ^ Integer operation
      -> m LLVM.Operand
numBinopCodegen x y ty opDouble opInt =
  case unfix ty of
    TyNumber numTy ->
      if numTy == NumInt then binaryOpCodegen x y opInt
                         else binaryOpCodegen x y opDouble
    _ -> error "Invariant violated"

-- | Generates code for BinOp
binOpCodegen :: LLVMIR.MonadIRBuilder m
            => BinaryOp -- ^ Operation
            -> Type -- ^ Type of the inputs
            -> m LLVM.Operand
            -> m LLVM.Operand
            -> m LLVM.Operand
binOpCodegen op ty x y =
  let (floatInstr, intInstr) = case op of
        Add -> (LLVMIR.fadd, LLVMIR.add)
        Sub -> (LLVMIR.fsub, LLVMIR.sub)
        Mul -> (LLVMIR.fmul, LLVMIR.mul)
        Div -> (LLVMIR.fdiv, LLVMIR.sdiv)
        Lt -> (LLVMIR.fcmp LLVMFP.OLT, LLVMIR.icmp LLVMIP.SLT)
        Lte -> (LLVMIR.fcmp LLVMFP.OLE, LLVMIR.icmp LLVMIP.SLE)
        Equal -> (LLVMIR.fcmp LLVMFP.OEQ, LLVMIR.icmp LLVMIP.EQ)
  in numBinopCodegen x y ty floatInstr intInstr

-- | Generates code for a [JValue].
jvalueCodegen
        :: (LLVMIR.MonadIRBuilder m, MonadState CodegenTable m)
        => JValue
        -> m LLVM.Operand
jvalueCodegen = \case
        JVar name -> gets (snd . fromJust . Map.lookup name . tableVars)
        JLit l    -> literalCodegen l

-- | Generates code for a CFE.
controlFlowCodegen
        :: HasType fields
        => JValue -- ^ The value to continue control flow with.
        -> LLVM.Operand -- ^ The LLVM operand of the value.
        -> ControlFlow (AnnExpr fields) -- ^ The control flow continuation.
        -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) ()
controlFlowCodegen val valOper = \case
  JIf (Cfe trueBr trueCf) (Cfe falseBr falseCf) -> do
    LLVMIR.ensureBlock
    trueLabel <- LLVMIR.freshName "if_then"
    falseLabel <- LLVMIR.freshName "if_else"
    LLVMIR.condBr valOper trueLabel falseLabel
    LLVMIR.emitBlockStart trueLabel
    (trueVal, trueOper) <- jexprCodegen trueBr
    _ <- controlFlowCodegen trueVal trueOper trueCf
    LLVMIR.emitBlockStart falseLabel
    (falseVal, falseOper) <- jexprCodegen falseBr
    _ <- controlFlowCodegen falseVal falseOper falseCf
    pure ()
  JCase branches -> do
    LLVMIR.ensureBlock
    defLabel <- LLVMIR.freshName "case_default"
    allCaseLabels <- forM branches $ \case
      BrAdt name _ _ ->
        let labelName = "case_" <> fromString (Text.unpack name) in
        (name, ) <$> LLVMIR.freshName labelName
    -- Assume the symbol table and type information is correct
    dataName <- (\case { TyAdt n -> n; _ -> error "" }) <$> lookupValueType val
    ctors <- gets ((\(_,_,cs) -> cs) . fromJust . Map.lookup dataName . tableAdts)
    let ctorNames = ctorGetName <$> ctors
    let usedLabelTriples = filter (\(_, (n, _)) -> n `elem` ctorNames) $ [0..] `zip` allCaseLabels
    let jumpTable = [(LLVMC.Int 32 i, l) | (i, (_, l)) <- usedLabelTriples]
    tag <- LLVMIR.extractValue valOper [0]
    dataPtr <- LLVMIR.extractValue valOper [1]
    LLVMIR.switch tag defLabel jumpTable
    forM_ (usedLabelTriples `zip` branches) $ \((_, (ctorName, label)), br) -> do
      let expr = branchGetExpr br
      let cf = branchGetControlFlow br
      (_, ctorLLVMName, index) <- gets (fromJust . Map.lookup ctorName . tableCtors)
      let Ctor _ argTys = ctors !! index
      let bindingPairs = branchGetBindings br `zip` (typeToLLVM <$> argTys)
      LLVMIR.emitBlockStart label
      ctorPtr <- LLVMIR.bitcast dataPtr (LLVM.ptr (LLVM.NamedTypeReference ctorLLVMName))
      ctorPtrOffset <- LLVMIR.int32 0
      bindings <- forM ([0..] `zip` bindingPairs) $ \(i, (n, llvmTy)) -> do
        ctorPtrIndex <- LLVMIR.int32 i
        -- Need to bitcast the ptr type because we need a concrete type. We
        -- also need to load the data immediately because of how variables
        -- are implemented.
        v <- LLVMIR.gep ctorPtr [ctorPtrOffset, ctorPtrIndex]
             >>= flip LLVMIR.bitcast (LLVM.ptr llvmTy)
             >>= flip LLVMIR.load 0
        ty <- lookupValueType (JVar n)
        pure (n, (ty, v))
      let updateTable t = t { tableVars = Map.union (tableVars t) (Map.fromList bindings) }
      (exprVal, exprOper) <- jexprCodegen expr
      localCodegenTable updateTable (controlFlowCodegen exprVal exprOper cf)
    LLVMIR.emitBlockStart defLabel
    LLVMIR.unreachable
  JJump lbl -> do
    v <- jvalueCodegen val
    jvals <- gets tableJoinValues
    block <- LLVMIR.currentBlock
    let f (n, jvs) = Just (n, (v, block) : jvs)
    let updJvals = Map.update f lbl jvals
    modify (\t -> t { tableJoinValues = updJvals })
    llvmLabel <- gets (fst . fromJust . Map.lookup lbl . tableJoinValues)
    LLVMIR.br llvmLabel

-- | Generates code for a given callable (i.e. in a JApp)
callableCodegen
        :: Callable -- ^ The callable
        -> [JValue] -- ^ The argument values
        -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) LLVM.Operand
callableCodegen callable args = case callable of
  CFunc name -> do
   fn <- fromJust <$> lookupName name
   ops <- traverse jvalueCodegen args
   LLVMIR.call fn [(x, []) | x <- ops]
  CBinOp op -> case args of
    [x, y] -> do
      xTy <- lookupValueType x
      binOpCodegen op (Fix xTy) (jvalueCodegen x) (jvalueCodegen y)
    _ -> error $ "callableCodegen: expected 2 args to CBinOp, got " ++ show (length args)
  CCast targetNum -> case args of
    [x] -> do
      ty <- lookupValueType x
      case ty of
        TyNumber srcNum -> jvalueCodegen x >>= castOpCodegen srcNum targetNum
        _ -> error $ "callableCodegen: expected CCast argument to be of numeric type, got " ++ show ty
    _ -> error $ "callableCodegen: expected 1 args to CCast, got " ++ show (length args)
  CCtor name -> do
    -- Assume constructor exists, since typechecker should verify it anyways
    (dataTy, ctorName, ctorIndex) <- gets (fromJust . Map.lookup name . tableCtors)
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
    ctorStructPtr <- LLVMIR.call RT.mallocRef [(ctorStructSize, [])] >>=
                     flip LLVMIR.bitcast (LLVM.ptr ctorTy)
    values <- traverse jvalueCodegen args
    indices <- traverse LLVMIR.int32 [0..fromIntegral (length values - 1)]
    ptrOffset <- LLVMIR.int32 0
    forM_ (indices `zip`  values) $ \(index, v) -> do
      valuePtr <- LLVMIR.gep ctorStructPtr [ptrOffset, index]
      LLVMIR.store valuePtr 0 v
      pure ()
    dataPtr <- LLVMIR.bitcast ctorStructPtr (LLVM.ptr LLVM.i8)
    fullyInit <- LLVMIR.insertValue tagStruct2 dataPtr [1]
    pure fullyInit
  CPrint -> case args of
    [val] -> do
      let fmtStr = encodeUtf8 (fromString "Print: %s\n\0")
      let fmtStrLen = toInteger (BS.length fmtStr)
      let fmtStrData = LLVMC.Int 8 . toInteger <$> BS.unpack fmtStr
      fmtStrOper <- LLVMIR.array fmtStrData
      fmtStrPtr <- LLVMIR.alloca (LLVM.ArrayType (fromInteger fmtStrLen) LLVM.i8) Nothing 0
      _ <- LLVMIR.store fmtStrPtr 0 fmtStrOper
      printf <- gets tablePrintf
      str <- jvalueCodegen val
      exprCstring <- LLVMIR.call RT.stringCstringRef [(str, [])]
      fmtStrPtr' <- LLVMIR.bitcast fmtStrPtr (LLVM.ptr LLVM.i8)
      _ <- LLVMIR.call printf [(fmtStrPtr', []), (exprCstring, [])]
      LLVMIR.int64 0
    _ -> error $ "callableCodegen: expected 1 args to CPrint, got " ++ show (length args)
  CFunRef name -> gets (fromJust . Map.lookup name . tableFuns)

-- | Generates code for a [JExpr]
jexprCodegen
        :: HasType fields
        => AnnExpr fields
        -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) (JValue, LLVM.Operand)
jexprCodegen = (\e -> go (unfix (getType e)) (annGetExpr e)) . unfix
  where
    go :: HasType fields
       => TypeF Type
       -> JExprF (AnnExpr fields)
       -> LLVMIR.IRBuilderT (LLVMIR.ModuleBuilderT Codegen) (JValue, LLVM.Operand)
    go exprTy = \case
      JVal v -> (v,) <$> jvalueCodegen v
      JLet name val next -> do
        oper <- jvalueCodegen val
        _ <- bindVariable name exprTy oper
        jexprCodegen next
      JJoin lbl varName (Cfe expr cf) next -> do
        llvmLabel <- LLVMIR.freshName (fromString (Text.unpack lbl))
        let addJoinEntry t =
              t { tableJoinValues = Map.insert lbl (llvmLabel, []) (tableJoinValues t) }
        oldJoinEntries <- gets tableJoinValues
        (lastVal, lastValOper) <- jexprCodegen expr
        _ <- localCodegenTable addJoinEntry (controlFlowCodegen lastVal lastValOper cf)
        (_, joinValues) <- gets (fromJust . Map.lookup lbl . tableJoinValues)
        modify (\t -> t { tableJoinValues = oldJoinEntries })
        LLVMIR.emitBlockStart llvmLabel
        op <- LLVMIR.phi joinValues
        bindVariable varName exprTy op
        jexprCodegen next
      JApp varName callable args next -> do
        oper <- callableCodegen callable args
        bindVariable varName exprTy oper
        jexprCodegen next

-- | Generates code for numeric casting
castOpCodegen :: LLVMIR.MonadIRBuilder m => Numeric -> Numeric -> LLVM.Operand -> m LLVM.Operand
castOpCodegen source castTo oper = case (source, castTo) of
  (NumDouble, NumDouble) -> pure oper
  (NumInt, NumInt) -> pure oper
  (NumDouble, NumInt) -> LLVMIR.fptosi oper LLVM.i64
  (NumInt, NumDouble) -> LLVMIR.sitofp oper LLVM.double
  (NumUnknown, _) -> castOpCodegen NumDouble castTo oper
  (_, NumUnknown) -> error "castOpCodegen: attempting to cast to NumUnknown"

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
      TyString -> LLVM.ptr RT.stringType
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
funToLLVM :: HasType fields
          => Text
          -> [(Text, Type)]
          -> Type
          -> AnnExpr fields
          -> LLVMIR.ModuleBuilderT Codegen LLVM.Operand
funToLLVM name params ty body =
    let name' = if name == "main" then "__simpl_main" else name
        ftype = typeToLLVM ty
        fname = llvmName name'
        fparams = [(typeToLLVM t, fromString (Text.unpack n)) | (n, t) <- params]
    in mdo foper <- LLVMIR.function fname fparams ftype $ \args -> do
             LLVMIR.ensureBlock
             -- We need to make sure we don't pollute other function scopes
             oldVars <- gets tableVars
             let updVars t = tableVars t `Map.union` Map.fromList [(n, (unfix vty, op)) | ((n, vty), op) <- params `zip` args]
             modify (\t -> t { tableFuns = Map.insert name foper (tableFuns t)
                             , tableVars = updVars t })
             (_, retval) <- jexprCodegen body
             -- Restore old scope
             modify (\t -> t { tableVars = oldVars })
             LLVMIR.ret retval
           pure foper

-- | Generate code for the entire module
moduleCodegen :: HasType fields
              => String
              -> SymbolTable (AnnExpr fields)
              -> LLVMIR.ModuleBuilderT Codegen ()
moduleCodegen srcCode symTab = mdo
  -- Message is "Hi\n" (with null terminator)
  (_, msg) <- staticString ".message" "Hello world!\n"
  (_, resultFmt) <- staticString ".resultformat" "Result: %i\n"
  (_, exprSrc) <- staticString ".sourcecode" $ "Source code: " ++ srcCode ++ "\n"
  RT.emitRuntimeDecls
  modify (\t -> t { tablePrintf = RT.printfRef })
  forM_ (Map.toList . symTabAdts $ symTab) $ \(name, (_, ctors)) ->
    adtToLLVM name ctors
  -- Insert function operands into symbol table before emitting so order of
  -- definition doesn't matter. This works because the codegen monad is lazy.
  modify (\t -> t { tableFuns = tableFuns t `Map.union` Map.fromList funOpers })
  funOpers <- forM (Map.toList . symTabFuns $ symTab) $ \(name, (params, ty, body)) ->
    (name, ) <$> funToLLVM name params ty body

  _ <- LLVMIR.function "main" [] LLVM.i64 $ \_ -> do
    diagnosticsEnabled <- gets (enableDiagnostics . tableOptions)
    when diagnosticsEnabled $ LLVMIR.call RT.printfRef [(msg, [])] >> pure ()
    let mainTy = LLVM.ptr (LLVM.FunctionType LLVM.i64 [] False)
    let mainName = LLVM.mkName "__simpl_main"
    let mainRef = LLVM.ConstantOperand (LLVMC.GlobalReference mainTy mainName)
    when diagnosticsEnabled $ LLVMIR.call RT.printfRef [(exprSrc, [])] >> pure ()
    exprResult <- LLVMIR.call mainRef []
    when diagnosticsEnabled $ LLVMIR.call RT.printfRef [(resultFmt, []), (exprResult, [])] >> pure ()
    retcode <- LLVMIR.int64 1
    LLVMIR.ret retcode
  pure ()

runCodegen :: HasType fields => CompilerOpts -> String -> SymbolTable (AnnExpr fields) -> LLVM.Module
runCodegen opts srcCode symTab
  = runIdentity
  . flip evalStateT emptyCodegenTable
  . unCodegen
  . LLVMIR.buildModuleT "simpl.ll"
  $ lift (initCodegenTable opts symTab) >> moduleCodegen srcCode symTab
