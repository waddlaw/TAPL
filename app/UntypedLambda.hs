module Main (main) where

import           Language.UntypedLambda
import           Language.UntypedLambda.Prelude   (prelude)
import           Language.Utils

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           System.Console.Haskeline

type UntypedLambda = InputT (StateT Env IO) ()

data Env = Env
  { strategy :: Strategy
  , isTrace  :: Bool
  }

defaultEnvironment :: Env
defaultEnvironment = Env NormalOrder False

main :: IO ()
main = do
  putStrLn "Start untyped lambnda repl"

  _ <- runStateT (runInputT defaultSettings main') defaultEnvironment

  putStrLn "Leaving untyped lambda repl"

main' :: UntypedLambda
main' = do
  minput <- getInputLine "UntypedLambda> "
  case trim <$> minput of
    Nothing       -> return ()
    Just ":q"     -> return ()
    Just ":list"  -> listCmd >> main'
    Just ":trace" -> traceCmd >> main'
    Just input    -> evalCmd input >> main'

traceCmd :: UntypedLambda
traceCmd = do
  env <- getEnv
  putEnv $ env { isTrace = not $ isTrace env }

listCmd :: UntypedLambda
listCmd = outputStrLn $ renderPrelude prelude

evalCmd :: String -> UntypedLambda
evalCmd input = do
  istrace <- isTrace <$> getEnv

  case runUlParser input of
    Left err -> outputStrLn err
    Right term ->
      if istrace then
        do
          _ <- lift $ lift $ trace NormalOrder term
          return ()
      else
        outputStrLn $ render $ eval NormalOrder term

getEnv :: InputT (StateT a IO) a
getEnv = lift get

putEnv :: s -> InputT (StateT s IO) ()
putEnv = lift . put
