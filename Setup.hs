module Main (main) where

import           Distribution.Simple
import           Distribution.Simple.Setup          (BuildFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo,
                                                     emptyHookedBuildInfo)
import           Hpack

main :: IO ()
main = do
  let myHook = simpleUserHooks {
    preBuild = pbHpack
  }
  defaultMainWithHooks myHook

pbHpack :: Args -> BuildFlags -> IO HookedBuildInfo
pbHpack _ _ = do
  hpack Verbose defaultOptions
  return emptyHookedBuildInfo
