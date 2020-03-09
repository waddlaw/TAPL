module LambdaRepl.Types
  ( LambdaREPL
  , ReplCmd (..)
  , defaultReplCmd
  , ReplAction (..)
  , ReplEnv (..)
  , ParseFunc
  , EvalFunc
  , TraceFunc
  )
where

import Language.Core.Types
import RIO
import RIO.Orphans ()
import RIO.Process
import System.Console.Haskeline hiding (display)

type LambdaREPL = InputT (RIO ReplEnv) ()

data ReplCmd =
  ReplCmd
    { replCmdSet   :: ReplAction
    , replCmdUnset :: ReplAction
    , replCmdList  :: ReplAction
    , replCmdEnv   :: ReplAction
    , replCmdEval  :: ReplAction
    , replCmdTc    :: ReplAction
    , replCmdHelp  :: ReplAction
    , replCmdQuit  :: ReplAction
    }

defaultReplCmd :: ReplCmd
defaultReplCmd =
  ReplCmd
    { replCmdSet   = NoAction
    , replCmdUnset = NoAction
    , replCmdList  = NoAction
    , replCmdEnv   = NoAction
    , replCmdEval  = NotImplemented
    , replCmdTc    = NotImplemented
    , replCmdHelp  = Help
    , replCmdQuit  = Quit
    }

data ReplAction
  = NotImplemented
  | Help
  | Action (Text -> LambdaREPL)
  | ActionNoArg LambdaREPL
  | NoAction
  | Quit

type ParseFunc term = String -> Either String term
type EvalFunc  term = Strategy -> term -> term
type TraceFunc term = Strategy -> term -> [term]

data ReplEnv
  = ReplEnv
      { appLogFunc        :: LogFunc
      , appProcessContext :: ProcessContext
      , appStrategy       :: IORef Strategy
      , appIsTrace        :: IORef Bool
      }

instance HasLogFunc ReplEnv where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext ReplEnv where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
