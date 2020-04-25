{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.B
import RIO

main :: IO ()
main =
  execLambdaRepl "booleans" "B" $
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
