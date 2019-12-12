{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- Suppress LLVM sum type of records AST warnings

module Simpl.Backend.Runtime where

import Control.Monad (forM_, join)

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Linkage as LLVM
-- import qualified LLVM.AST.FloatingPointPredicate as LLVMFP
-- import qualified LLVM.AST.IntegerPredicate as LLVMIP
import qualified LLVM.AST.Constant as LLVMC
import qualified LLVM.AST.Global as LLVMG
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.IRBuilder.Module as LLVMIR
-- import qualified LLVM.IRBuilder.Monad as LLVMIR
-- import qualified LLVM.IRBuilder.Instruction as LLVMIR
-- import qualified LLVM.IRBuilder.Constant as LLVMIR

-- * Utility functions

-- | Represents a function type
type FunType = ([(String, LLVM.Type)], LLVM.Type, Bool)

mkFunType :: [(String, LLVM.Type)] -> LLVM.Type -> FunType
mkFunType args res = (args, res, False)

-- | Constructs the LLVM type representing the given function type
funTypeToLLVM :: FunType -> LLVM.Type
funTypeToLLVM (args, res, isVarArgs) = LLVM.ptr $ LLVM.FunctionType
  { LLVM.resultType = res
  , LLVM.argumentTypes = [ ty | (_, ty) <- args ]
  , LLVM.isVarArg = isVarArgs
  }

-- | Emits the given extern function declaration
emitRuntimeFun :: LLVMIR.MonadModuleBuilder m => String -> FunType -> m LLVM.Operand
emitRuntimeFun name funTy@(args, resTy, isVarArgs) = do
  LLVMIR.emitDefn $ LLVM.GlobalDefinition LLVMG.functionDefaults
    { LLVMG.name = LLVM.mkName name
    , LLVMG.linkage = LLVM.External
    , LLVMG.parameters = ([LLVM.Parameter t (LLVM.mkName n) [] | (n, t) <- args], isVarArgs)
    , LLVMG.returnType = resTy }
  pure (runtimeFunRef name funTy)

-- | The opaque type representing the struct with the given name defined in the
-- runtime
runtimeStruct :: String -> LLVM.Type
runtimeStruct name = LLVM.NamedTypeReference (LLVM.mkName ("struct." ++ name))

-- | An Operand that refers to the given function declared in the runtime
runtimeFunRef :: String  -- ^ Name of the runtime function
              -> FunType -- ^ The type of the function
              -> LLVM.Operand
runtimeFunRef name funTy =
    LLVM.ConstantOperand $ LLVMC.GlobalReference ty (LLVM.mkName name)
  where
    ty = funTypeToLLVM funTy

-- * C standard library functions

mallocType, memcpyType, printfType :: FunType
mallocType = mkFunType [("ptr", LLVM.i64)] (LLVM.ptr LLVM.i8)
memcpyType = mkFunType [ ("dest", LLVM.ptr LLVM.i8)
                       , ("src", LLVM.ptr LLVM.i8)
                       , ("len", LLVM.i64) ]
                       LLVM.void
printfType = ([("", LLVM.ptr LLVM.i8)], LLVM.void, True)

mallocRef, memcpyRef, printfRef :: LLVM.Operand
mallocRef = runtimeFunRef "simpl_malloc" mallocType
memcpyRef = runtimeFunRef "memcpy" memcpyType
printfRef = runtimeFunRef "printf" printfType

cstdlibFuns :: [(String, FunType)]
cstdlibFuns = [ ("simpl_malloc", mallocType)
              , ("memcpy", memcpyType)
              , ("printf", printfType) ]

-- * Strings

stringType :: LLVM.Type
stringType = runtimeStruct "simpl_string"

stringCstringType, stringNewType :: FunType
stringCstringType = mkFunType [("str", LLVM.ptr stringType)] (LLVM.ptr LLVM.i8)
stringNewType = mkFunType [("byte_count", LLVM.i64), ("data", LLVM.ptr LLVM.i8)] (LLVM.ptr stringType)

stringCstringRef, stringNewRef :: LLVM.Operand
stringCstringRef = runtimeFunRef "simpl_string_cstring" stringCstringType
stringNewRef = runtimeFunRef "simpl_string_new" stringNewType

stringFuns :: [(String, FunType)]
stringFuns = [ ("simpl_string_cstring", stringCstringType)
             , ("simpl_string_new", stringNewType)]

stringStructs :: [String]
stringStructs = ["simpl_string"]

-- * Tags

typeTagType :: LLVM.Type
typeTagType = runtimeStruct "simpl_type_tag"

taggedValueType :: LLVM.Type
taggedValueType = runtimeStruct "simpl_tagged_value"

tagSizeType, taggedTagType, taggedUnboxType :: FunType
tagSizeType = mkFunType [("t", LLVM.ptr typeTagType)] LLVM.i64
taggedTagType = mkFunType [("t", LLVM.ptr taggedValueType)] (LLVM.ptr taggedValueType)
taggedUnboxType = mkFunType [("t", LLVM.ptr taggedValueType)] (LLVM.ptr LLVM.void)

tagSizeRef, taggedTagRef, taggedUnboxRef :: LLVM.Operand
tagSizeRef = runtimeFunRef "simpl_tag_size" tagSizeType
taggedTagRef = runtimeFunRef "simpl_tagged_tag" taggedTagType
taggedUnboxRef = runtimeFunRef "simpl_tagged_unbox" taggedUnboxType

runtimeTypeFuns :: [(String, FunType)]
runtimeTypeFuns = [ ("simpl_tag_size", tagSizeType)
                  , ("simpl_tagged_tag", taggedTagType)
                  , ("simpl_tagged_unbox", taggedUnboxType) ]

runtimeTypeStructs :: [String]
runtimeTypeStructs = ["simpl_type_tag", "simpl_tagged_value"]

-- * Entire runtime

allRuntimeFuns :: [(String, FunType)]
allRuntimeFuns = join [cstdlibFuns, stringFuns, runtimeTypeFuns]

allRuntimeStructs :: [String]
allRuntimeStructs = stringStructs ++ runtimeTypeStructs

emitRuntimeDecls :: LLVMIR.MonadModuleBuilder m => m ()
emitRuntimeDecls = do
  forM_ allRuntimeFuns (uncurry emitRuntimeFun)
  forM_ allRuntimeStructs $ \name -> do
    LLVMIR.typedef (LLVM.mkName ("struct." <> name)) Nothing
