module Main (main) where

import           Language.NB
import           Language.Utils

import           System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Start NB repl"

  _ <- runInputT defaultSettings main'

  putStrLn "Leaving NB repl"

main' :: InputT IO ()
main' = do
  minput <- getInputLine "NB> "
  case trim <$> minput of
    Nothing       -> return ()
    Just ":q"     -> return ()
    Just input -> evalCmd input >> main'

evalCmd :: String -> InputT IO ()
evalCmd str =
  case runNbParser str of
    Left err -> outputStrLn err
    Right t  -> outputStrLn $ render $ eval t