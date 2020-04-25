{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.SimpleLambda as SimpleLambda
import RIO
import System.Console.Haskeline hiding (display)

main :: IO ()
main =
  runApp $ do
    logInfo "Start simple lambda repl"
    logInfo ":help for a list of commands"
    _ <- runInputT defaultSettings main'
    logInfo "Leaving simple lambda repl"

main' :: LambdaREPL
main' = repl "SimpleLambda" commands
  where
    commands =
      defaultReplCmd
        { replCmdEval = Action (evalCmd (parser mempty) evaluator tracer),
          replCmdTc = Action (tcCmd (parser mempty) typecheck)
        }

parser :: Context -> ParseFunc SimpleLambda.Term
parser = runSimpleLambdaParser

typecheck :: SimpleLambda.Term -> SimpleLambda.Ty
typecheck = SimpleLambda.typeof mempty

evaluator :: EvalFunc SimpleLambda.Term
evaluator _ = id -- FIXME

tracer :: TraceFunc SimpleLambda.Term
tracer = error ".........no..."
