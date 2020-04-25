{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import qualified Language.FJ as FJ
import RIO
import System.Console.Haskeline hiding (display)

main :: IO ()
main =
  runApp $ do
    logInfo "Start fetherweight-java repl"
    logInfo ":help for a list of commands"
    _ <- runInputT defaultSettings main'
    logInfo "Leaving fetherweight-java repl"

main' :: LambdaREPL
main' = repl "FJ" commands
  where
    commands =
      defaultReplCmd
        { replCmdEval = Action (evalCmd parser evaluator tracer),
          replCmdTc = NotImplemented
        }

parser :: ParseFunc FJ.Term
parser = FJ.runFjParser

-- typecheck :: FJ.Term -> FJ.Ty
-- typecheck =  error ".........no..."

evaluator :: EvalFunc FJ.Term
evaluator _ = FJ.eval FJ.exCT

tracer :: TraceFunc FJ.Term
tracer _ = FJ.evalTrace FJ.exCT
