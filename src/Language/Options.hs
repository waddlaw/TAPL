{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.Options
  ( evalCmd
  , tcCmd
  , helpCmd
  ) where

import           RIO                       hiding (trace)
import qualified RIO.Text                  as Text

import           Language.Types
import           Language.Utils

import           Data.Text.Prettyprint.Doc

evalCmd :: Pretty term => ParseFunc term -> EvalFunc term -> Text -> RIO ReplEnv ()
evalCmd parser evalate input = ask >>= \ReplEnv{..} ->
  case parser input of
    Left err -> logError $ display $ Text.pack err
    Right term ->
      if appIsTrace
      then
          -- _ <- liftIO $ trace appStrategy term
          return ()
      else
        logInfo $ display $ Text.pack $ render $ evalate appStrategy term

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