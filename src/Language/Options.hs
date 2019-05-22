{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Options
  ( runApp
  , evalCmd
  , tcCmd
  , helpCmd
  ) where

import RIO hiding (trace)
import RIO.Process
import qualified RIO.Text as Text

import Language.Core
import Language.Types

import Data.Text.Prettyprint.Doc
import System.Environment

runApp :: MonadIO m => RIO ReplEnv a -> m a
runApp m = liftIO $ do
  verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
  lo <- logOptionsHandle stderr verbose
  pc <- mkDefaultProcessContext
  strategy <- newIORef NormalOrder
  isTrace <- newIORef False
  withLogFunc lo $ \lf ->
    let app = ReplEnv
          { appLogFunc = lf
          , appProcessContext = pc
          , appStrategy = strategy
          , appIsTrace = isTrace
          }
     in runRIO app m

evalCmd :: Pretty term => ParseFunc term -> EvalFunc term -> TraceFunc term -> Text -> RIO ReplEnv ()
evalCmd parser evaluator tracer input = ask >>= \ReplEnv{..} -> do
  strategy <- readIORef appStrategy
  isTrace <- readIORef appIsTrace
  
  case parser input of
    Left err -> logError $ display $ Text.pack err
    Right term ->
      if isTrace
      then logInfo $ displayRender $ tracer strategy term
      else logInfo $ displayRender $ evaluator strategy term

tcCmd :: Pretty t => ParseFunc term -> (term -> t) -> Text -> RIO ReplEnv ()
tcCmd parser checker input =
  case Text.stripPrefix ":t " input of
    Nothing -> do
      logInfo "コマンドがの書式が間違っています。"
      return ()
    Just input' ->
      case parser input' of
        Left err   -> logError $ display $ Text.pack err
        Right term -> logInfo $ display $ Text.pack $ render $ checker term

helpCmd :: HasLogFunc env => RIO env ()
helpCmd = mapM_ (logInfo . display) $ "available commands" : commands

commands :: [Text]
commands =
  [ "  :set trace               -- トレースの有効化 (簡約の途中経過も含めて表示)"
  , "  :set strategy <Strategy> -- 評価戦略の設定"
  , "  :unset trace             -- トレースの無効化"
  , "  :list strategy           -- 評価戦略の一覧を表示"
  , "  :list prelude            -- prelude 関数の一覧を表示"
  , "  :env                     -- 現在の設定内容を表示"
  , "  :help                    -- ヘルプ"
  , "  :q                       -- 終了"
  ]
