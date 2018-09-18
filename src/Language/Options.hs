{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Options
  ( evalCmd
  , helpCmd
  ) where

import RIO hiding (trace)
import qualified RIO.Text as Text

import Language.UntypedLambda
import Language.UntypedLambda.Types
import Language.Utils
import Language.Types

import           Data.Text.Prettyprint.Doc

evalCmd :: Pretty term => ParseFunc term -> EvalFunc term -> Text -> RIO ReplEnv ()
evalCmd parser eval input = ask >>= \ReplEnv{..} -> do
  case parser input of
    Left err -> logError $ display $ Text.pack err
    Right term ->
      if appIsTrace
      then do
          -- _ <- liftIO $ trace appStrategy term
          return ()
      else
        logInfo $ display $ Text.pack $ render $ eval appStrategy term

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