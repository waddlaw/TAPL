{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.B
import RIO
import System.Console.Haskeline hiding (display)

main :: IO ()
main =
  runApp $ do
    logInfo "Start booleans (B) repl"
    logInfo ":help for a list of commands"
    _ <- runInputT defaultSettings main'
    logInfo "Leaving booleans (B) repl"

main' :: LambdaREPL
main' = repl "B" commands
  where
    commands =
      defaultReplCmd
        { replCmdEnv = NotImplemented,
          replCmdEval = Action (evalCmd parser evaluator tracer),
          replCmdTc = NotImplemented
        }

parser :: ParseFunc Term
parser = runLangParser pTerm

evaluator :: EvalFunc Term
evaluator _ = eval

tracer :: TraceFunc Term
tracer _ = evalTrace
