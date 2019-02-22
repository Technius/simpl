module Simpl.CompilerOptions where

-- | Compiler options
data CompilerOpts = MkCompilerOpts
  { -- | Whether diagnostics should be printed at runtime
    enableDiagnostics :: Bool
  } deriving (Show)

defaultCompilerOpts :: CompilerOpts
defaultCompilerOpts = MkCompilerOpts
  { enableDiagnostics = False }
