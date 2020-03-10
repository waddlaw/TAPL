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
      { replCmdList = Action subCmdList
      , replCmdEval = Action (evalCmd parser evaluator tracer)
      , replCmdTc   = NoAction
      }

subCmdList :: Text -> LambdaREPL
subCmdList input =
  case subCmd "list" input of
    "strategy" -> listStrategyCmd
    "prelude"  -> listPreludeCmd prelude'
    _  -> return ()

parser :: ParseFunc UntypedLambda
parser = UntypedLambda.runUlParser

evaluator :: EvalFunc UntypedLambda
evaluator = UntypedLambda.eval

tracer :: TraceFunc UntypedLambda
tracer = UntypedLambda.trace

prelude' :: Prelude UntypedLambda
prelude' = UntypedLambda.prelude
