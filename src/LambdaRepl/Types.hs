module LambdaRepl.Types
  ( LambdaREPL,
    ReplEnv (..),
    ParseFunc,
    EvalFunc,
    TraceFunc
    )
where

import Language.Core.Types
import RIO
import RIO.Process
import System.Console.Haskeline hiding (display)

type LambdaREPL = InputT (RIO ReplEnv) ()

type ParseFunc term = String -> Either String term

type EvalFunc term = Strategy -> term -> term

type TraceFunc term = Strategy -> term -> [term]

data ReplEnv
  = ReplEnv
      { appLogFunc :: !LogFunc,
        appProcessContext :: !ProcessContext,
        appStrategy :: IORef Strategy,
        appIsTrace :: IORef Bool
        }

instance HasLogFunc ReplEnv where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext ReplEnv where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
