module Main (main) where

import           Language.UntypedLambda
import           Language.UntypedLambda.Prelude (prelude)
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
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just ":list" -> listCmd >> main'
    Just input   -> evalCmd input >> main'

listCmd :: InputT IO ()
listCmd = outputStrLn $ renderPrelude prelude

evalCmd :: String -> InputT IO ()
evalCmd = outputStrLn . either id (render . eval NormalOrder) . runUlParser