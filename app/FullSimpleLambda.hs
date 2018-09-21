{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           RIO
import           RIO.Process
import qualified RIO.Text                     as Text

import           Language.Orphans             ()
import           Language.Options
import           Language.Types
import           Language.Utils

import           Language.FullSimpleLambda

import qualified Language.UntypedLambda.Types as UntypedLambda

import           System.Console.Haskeline     hiding (display)
import           System.Environment

type FullSimpleLambdaREPL = InputT (RIO ReplEnv) ()

runApp :: MonadIO m => RIO ReplEnv a -> m a
runApp m = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = ReplEnv
          { appLogFunc = lf
          , appProcessContext = pc
          , appStrategy = UntypedLambda.NormalOrder
          , appIsTrace = False
          }
     in runRIO app m

main :: IO ()
main = runApp $ do
  logInfo "Start full simple lambnda repl"
  logInfo ":help でコマンドの一覧が確認できます。"

  _ <- runInputT defaultSettings main'

  logInfo "Leaving full simple lambda repl"

main' :: FullSimpleLambdaREPL
main' = do
  minput <- getInputLine "FullSimpleLambda> "
  case Text.pack . trim <$> minput of
    Nothing      -> return ()
    Just ":q"    -> return ()
    Just input ->
      if  | ":help" `Text.isPrefixOf` input -> lift helpCmd >> main'
          | ":t" `Text.isPrefixOf` input -> lift (tcCmd (parser mempty) typecheck input) >> main'
          | otherwise -> do
              lift (evalCmd (parser mempty) eval input)
              main'

parser :: Context -> Text -> Either String Term
parser ctx = runFullSimpleLambdaParser ctx . Text.unpack

typecheck :: Term -> Ty
typecheck = undefined

eval :: EvalFunc Term
eval _ = undefined
