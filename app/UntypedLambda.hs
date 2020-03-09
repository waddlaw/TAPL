{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.Core
import Language.UntypedLambda as UntypedLambda
import RIO
import System.Console.Haskeline hiding (display)

main :: IO ()
main =
  runApp $ do
    logInfo "Start untyped lambda repl"
    logInfo ":help for a list of commands"
    _ <- runInputT defaultSettings main'
    logInfo "Leaving untyped lambda repl"

main' :: LambdaREPL
main' = repl "UntypedLambda" commands
  where
    commands = defaultReplCmd
      { replCmdSet   = Action subCmdSet
      , replCmdUnset = Action subCmdUnset
      , replCmdList  = Action subCmdList
      , replCmdEnv   = ActionNoArg printEnvCmd
      , replCmdEval  = Action (evalCmd parser evaluator tracer)
      , replCmdTc    = NoAction
      }
    subCmdSet input = case subCmd "set" input of
      "trace"    -> updateEnvTraceCmd True
      "strategy" -> updateEnvStrategyCmd input
      _ -> return ()
    subCmdUnset input = case subCmd "unset" input of
      "trace" -> updateEnvTraceCmd False
      _ -> return ()
    subCmdList input = case subCmd "list" input of
      "strategy" -> listStrategyCmd
      "prelude"  -> (listPreludeCmd prelude')
      _  -> return ()

parser :: ParseFunc UntypedLambda
parser = UntypedLambda.runUlParser

evaluator :: EvalFunc UntypedLambda
evaluator = UntypedLambda.eval

tracer :: TraceFunc UntypedLambda
tracer = UntypedLambda.trace

prelude' :: Prelude UntypedLambda
prelude' = UntypedLambda.prelude
