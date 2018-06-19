{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import           Language.UntypedLambda
import           Language.UntypedLambda.Prelude   (prelude)
import           Language.Utils

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.List
import           System.Console.Haskeline

type UntypedLambda = InputT (StateT Env IO) ()

data Env = Env
  { envStrategy :: Strategy
  , envIsTrace  :: Bool
  } deriving Show

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
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just input ->
      if | ":set trace"     `isPrefixOf` input -> updateEnvTraceCmd True     >> main'
         | ":set strategy"  `isPrefixOf` input -> updateEnvStrategyCmd input >> main'
         | ":unset trace"   `isPrefixOf` input -> updateEnvTraceCmd False    >> main'
         | ":list strategy" `isPrefixOf` input -> listStrategyCmd            >> main'
         | ":list prelude"  `isPrefixOf` input -> listPreludeCmd             >> main'
         | ":env"           `isPrefixOf` input -> printEnvCmd                >> main'
         -- main process
         | otherwise -> evalCmd input >> main'

printEnvCmd :: UntypedLambda
printEnvCmd = do
  env <- getEnv
  outputStrLn $ show env

updateEnvTraceCmd :: Bool -> UntypedLambda
updateEnvTraceCmd isTrace = do
  env <- getEnv
  putEnv $ env { envIsTrace = isTrace }
  printEnvCmd

updateEnvStrategyCmd :: String -> UntypedLambda
updateEnvStrategyCmd input = do
  env <- getEnv
  putEnv $ env { envStrategy = strategy }
  printEnvCmd
  where
    strategy = read $ last $ words input

listStrategyCmd :: UntypedLambda
listStrategyCmd = mapM_ (outputStrLn . show) strategies
  where
    strategies = [minBound .. maxBound] :: [Strategy]

listPreludeCmd :: UntypedLambda
listPreludeCmd = outputStrLn $ renderPrelude prelude

evalCmd :: String -> UntypedLambda
evalCmd input = do
  isTrace<- envIsTrace <$> getEnv
  strategy <- envStrategy <$> getEnv

  case runUlParser input of
    Left err -> outputStrLn err
    Right term ->
      if isTrace then
        do
          _ <- lift $ lift $ trace strategy term
          return ()
      else
        outputStrLn $ render $ eval strategy term

getEnv :: InputT (StateT a IO) a
getEnv = lift get

putEnv :: s -> InputT (StateT s IO) ()
putEnv = lift . put
