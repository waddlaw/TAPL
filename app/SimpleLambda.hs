{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
module Main (main) where

import RIO
import           RIO.Process
import qualified RIO.Text as Text

import Language.Utils
import Language.Options
import Language.Types
import Language.UntypedLambda.Types
import Language.SimpleLambda.Types as SimpleLambda
import Language.SimpleLambda.Parser

import           System.Console.Haskeline hiding (display)
import System.Environment

instance MonadException (RIO env) where
  controlIO f = RIO $ controlIO $ \(RunIO run) -> let
    run' = RunIO (fmap RIO . run . unRIO)
    in fmap unRIO $ f run'

type SimpleLambdaREPL = InputT (RIO ReplEnv) ()

runApp :: MonadIO m => RIO ReplEnv a -> m a
runApp m = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = ReplEnv
          { appLogFunc = lf
          , appProcessContext = pc
          , appStrategy = NormalOrder
          , appIsTrace = False
          }
     in runRIO app m

main :: IO ()
main = runApp $ do
  logInfo "Start simple lambnda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving simple lambda repl"

main' :: SimpleLambdaREPL
main' = do
  minput <- getInputLine "SimpleLambda> "
  case Text.pack . trim <$> minput of
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just input ->
      if  | ":help" `Text.isPrefixOf` input -> lift helpCmd >> main'
          | otherwise -> lift (evalCmd parser eval input) >> main'

parser :: Text -> Either String SimpleLambda.Term
parser = runSimpleLambdaParser . Text.unpack

eval :: EvalFunc SimpleLambda.Term
eval _ = id -- FIXME