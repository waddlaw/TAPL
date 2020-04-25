module Main (main) where

import Language.Core
import Language.NB
import RIO
import System.Console.Haskeline
import Prelude (putStrLn)

main :: IO ()
main = do
  putStrLn "Start NB repl"
  _ <- runInputT defaultSettings main'
  putStrLn "Leaving NB repl"

main' :: InputT IO ()
main' = do
  minput <- getInputLine "NB> "
  case trim <$> minput of
    Nothing -> return ()
    Just ":q" -> return ()
    Just input -> evalCmd input >> main'

evalCmd :: String -> InputT IO ()
evalCmd = outputStrLn . either id (render . eval) . runNbParser
