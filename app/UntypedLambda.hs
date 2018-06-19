module Main (main) where

import           Language.UntypedLambda
import           Language.Utils

import           System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Start untyped lambnda repl"

  _ <- runInputT defaultSettings main'

  putStrLn "Leaving untyped lambda repl"

main' :: InputT IO ()
main' = do
  minput <- getInputLine "UntypedLambda> "
  case trim <$> minput of
    Nothing    -> return ()
    Just ":q"  -> return ()
    Just input -> evalCmd input >> main'

evalCmd :: String -> InputT IO ()
evalCmd = outputStrLn . either id (render . eval NormalOrder) . runUlParser
