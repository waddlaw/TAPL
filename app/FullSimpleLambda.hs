{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.Text as Text

import LambdaRepl

import Language.Core

import Language.FullSimpleLambda

import System.Console.Haskeline hiding (display)

main :: IO ()
main = runApp $ do
  logInfo "Start full simple lambnda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving full simple lambda repl"

main' :: LambdaREPL
main' = do
  minput <- getInputLine "FullSimpleLambda> "
  case Text.pack . trim <$> minput of
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just input ->
      if  | ":help" `Text.isPrefixOf` input -> lift helpCmd >> main'
          | ":t" `Text.isPrefixOf` input -> lift (tcCmd (parser mempty) typecheck input) >> main'
          | otherwise -> do
              lift (evalCmd (parser mempty) evaluator tracer input)
              main'

parser :: Context -> Text -> Either String Term
parser ctx = runFullSimpleLambdaParser ctx . Text.unpack

typecheck :: Term -> Ty
typecheck = typeof mempty

evaluator :: EvalFunc Term
evaluator _ = eval

tracer :: TraceFunc Term
tracer = error ".......NO"
