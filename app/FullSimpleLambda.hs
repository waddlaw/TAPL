{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.Text as Text

import LambdaRepl
import Language.Core
import Language.FullSimpleLambda as FullSimpleLambda

import System.Console.Haskeline hiding (display)

main :: IO ()
main = runApp $ do
  logInfo "Start full simple lambda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving full simple lambda repl"

main' :: LambdaREPL
main' = do
  minput <- getInputLine "FullSimpleLambda> "
  case Text.pack . trim <$> minput of
    Nothing    -> return ()
    Just ":q"  -> return ()
    Just input ->
      if  | ":help" `Text.isPrefixOf` input -> helpCmd >> main'
          | ":t" `Text.isPrefixOf` input -> tcCmd (parser mempty) typecheck input >> main'
          | otherwise -> evalCmd (parser mempty) evaluator tracer input >> main'

parser :: Context -> Text -> Either String FullSimpleLambda.Term
parser ctx = runFullSimpleLambdaParser ctx . Text.unpack

typecheck :: Term -> FullSimpleLambda.Ty
typecheck = typeof mempty

evaluator :: EvalFunc FullSimpleLambda.Term
evaluator _ = eval

tracer :: TraceFunc FullSimpleLambda.Term
tracer = error ".......NO"
