{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.Text as Text

import LambdaRepl
import Language.Core
import Language.UntypedLambda as UntypedLambda

import System.Console.Haskeline hiding (display)

main :: IO ()
main = runApp $ do
  logInfo "Start untyped lambda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving untyped lambda repl"

main' :: LambdaREPL
main' = do
  minput <- getInputLine "UntypedLambda> "
  case Text.pack . trim <$> minput of
    Nothing    -> return ()
    Just ":q"  -> return ()
    Just input ->
      if | ":set trace"     `Text.isPrefixOf` input -> updateEnvTraceCmd True     >> main'
         | ":set strategy"  `Text.isPrefixOf` input -> updateEnvStrategyCmd input >> main'
         | ":unset trace"   `Text.isPrefixOf` input -> updateEnvTraceCmd False    >> main'
         | ":list strategy" `Text.isPrefixOf` input -> listStrategyCmd            >> main'
         | ":list prelude"  `Text.isPrefixOf` input -> listPreludeCmd prelude'    >> main'
         | ":env"           `Text.isPrefixOf` input -> printEnvCmd                >> main'
         | ":help"          `Text.isPrefixOf` input -> helpCmd                    >> main'
         -- main process
         | otherwise -> evalCmd parser evaluator tracer input >> main'

parser :: ParseFunc UntypedLambda
parser = UntypedLambda.runUlParser

evaluator :: EvalFunc UntypedLambda
evaluator = UntypedLambda.eval

tracer :: TraceFunc UntypedLambda
tracer = UntypedLambda.trace

prelude' :: Prelude UntypedLambda
prelude' = UntypedLambda.prelude