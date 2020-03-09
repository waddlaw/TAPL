{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.FullSimpleLambda as FullSimpleLambda
import RIO
import System.Console.Haskeline hiding (display)

main :: IO ()
main =
  runApp $ do
    logInfo "Start full simple lambda repl"
    logInfo ":help for a list of commands"
    _ <- runInputT defaultSettings main'
    logInfo "Leaving full simple lambda repl"

main' :: LambdaREPL
main' = repl "FullSimpleLambda" commands
  where
    commands = defaultReplCmd
      { replCmdEval = Action (evalCmd (parser mempty) evaluator tracer)
      , replCmdTc   = Action (tcCmd (parser mempty) typecheck)
      }

parser :: Context -> ParseFunc FullSimpleLambda.Term
parser = runFullSimpleLambdaParser

typecheck :: Term -> FullSimpleLambda.Ty
typecheck = typeof mempty

evaluator :: EvalFunc FullSimpleLambda.Term
evaluator _ = eval

tracer :: TraceFunc FullSimpleLambda.Term
tracer = error ".......NO"
