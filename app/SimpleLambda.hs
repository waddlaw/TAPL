{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import RIO
import qualified RIO.Text as Text

import LambdaRepl
import Language.Core
import Language.SimpleLambda as SimpleLambda

import System.Console.Haskeline hiding (display)

main :: IO ()
main = runApp $ do
  logInfo "Start simple lambnda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving simple lambda repl"

main' :: LambdaREPL
main' = do
  minput <- getInputLine "SimpleLambda> "
  case Text.pack . trim <$> minput of
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just input ->
      if  | ":help" `Text.isPrefixOf` input -> helpCmd >> main'
          | ":t" `Text.isPrefixOf` input -> tcCmd (parser mempty) typecheck input >> main'
          | otherwise -> evalCmd (parser mempty) evaluator tracer input >> main'

parser :: Context -> Text -> Either String SimpleLambda.Term
parser ctx = runSimpleLambdaParser ctx . Text.unpack

typecheck :: SimpleLambda.Term -> SimpleLambda.Ty
typecheck = SimpleLambda.typeof mempty

evaluator :: EvalFunc SimpleLambda.Term
evaluator _ = id -- FIXME

tracer :: TraceFunc SimpleLambda.Term
tracer = error ".........no..."