module Main (main) where

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Program
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import Debug.Trace

main :: IO ()
main = defaultMainWithHooks (simpleUserHooks { hookedPreProcessors = [("fl", ppFluidToHaskell)] })

fluidToHaskellProgram :: Program
fluidToHaskellProgram =
  (simpleProgram "fltkhs-fluidtohs") { programFindVersion = (\_ _ -> return Nothing) }

ppFluidToHaskell :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppFluidToHaskell bi lbi =
  PreProcessor
    { platformIndependent = True
    , runPreProcessor = \(inBaseDir, inRelativeFile) (outBaseDir, outRelativeFile) verbosity -> do
        (fluidToHaskellProg, _) <- requireProgram verbosity fluidToHaskellProgram (withPrograms lbi)
        rawSystemProgram verbosity fluidToHaskellProg
          ["--output-dir=" ++ outBaseDir, inBaseDir </> inRelativeFile]
    }
