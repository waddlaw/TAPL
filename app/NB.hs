{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LambdaRepl
import Language.NB
import RIO

main :: IO ()
main =
  execLambdaRepl "arithmetic expressions" "NB" $
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
