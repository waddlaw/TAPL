{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.List as List
import qualified RIO.Text as Text

import Language.Core
import Language.Options
import Language.Orphans ()
import Language.Types

import Language.UntypedLambda (UntypedLambda)
import qualified Language.UntypedLambda as UntypedLambda

import System.Console.Haskeline hiding (display)

main :: IO ()
main = runApp $ do
  logInfo "Start untyped lambnda repl"
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
         | ":list prelude"  `Text.isPrefixOf` input -> listPreludeCmd             >> main'
         | ":env"           `Text.isPrefixOf` input -> printEnvCmd                >> main'
         | ":help"          `Text.isPrefixOf` input -> lift helpCmd               >> main'
         -- main process
         | otherwise -> lift (evalCmd parser evaluator tracer input) >> main'

parser :: Text -> Either String UntypedLambda
parser = UntypedLambda.runUlParser . Text.unpack

evaluator :: EvalFunc UntypedLambda
evaluator = UntypedLambda.eval

tracer :: TraceFunc UntypedLambda
tracer = UntypedLambda.trace

printEnvCmd :: LambdaREPL
printEnvCmd = lift $ ask >>= \ReplEnv{..} -> do
  strategy <- readIORef appStrategy
  isTrace <- readIORef appIsTrace
  let msg = Text.unlines [ "strategy: " <> tshow strategy
                         , "isTrace: " <> tshow isTrace
                         ]
  logInfo $ display msg

updateEnvTraceCmd :: Bool -> LambdaREPL
updateEnvTraceCmd newIsTrace = do
  lift $ ask >>= \ReplEnv{..} -> writeIORef appIsTrace newIsTrace
  printEnvCmd

updateEnvStrategyCmd :: Text -> LambdaREPL
updateEnvStrategyCmd input = do
  lift $ case readLastInput input of
    Nothing -> logError "Invalid strategy"
    Just newStrategy -> ask >>= \ReplEnv{..} ->
      writeIORef appStrategy newStrategy

  printEnvCmd

readLastInput :: Read a => Text -> Maybe a
readLastInput input = do
  lastInput <- List.lastMaybe $ Text.words input
  readMaybe $ Text.unpack lastInput

listStrategyCmd :: LambdaREPL
listStrategyCmd = mapM_ (outputStrLn . show) strategies

listPreludeCmd :: LambdaREPL
listPreludeCmd = outputStrLn $ renderPrelude UntypedLambda.prelude