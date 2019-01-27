import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Program ( requireProgram, runProgram, simpleProgram, defaultProgramDb
                                   , ProgArg, Program, ProgramDb, ConfiguredProgram )
import Distribution.Types.HookedBuildInfo
import Distribution.Verbosity (Verbosity, normal)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = preBuildHook
                                              , preClean = preCleanHook }

makeProgram :: Program
makeProgram = simpleProgram "make"

configuredMake :: Verbosity -> IO (ConfiguredProgram, ProgramDb)
configuredMake verb = requireProgram verb makeProgram defaultProgramDb

runMake :: Verbosity -> [ProgArg] -> IO ()
runMake verb args = do
  (make', progDb) <- configuredMake verb
  runProgram verb make' args

preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook _ bflags = do
  runMake (fromFlagOrDefault normal (buildVerbosity bflags)) ["-C", "runtime", "compile"]
  pure emptyHookedBuildInfo

preCleanHook :: Args -> CleanFlags -> IO HookedBuildInfo
preCleanHook _ cflags = do
  runMake (fromFlagOrDefault normal (cleanVerbosity cflags)) ["-C", "runtime", "clean"]
  pure emptyHookedBuildInfo
