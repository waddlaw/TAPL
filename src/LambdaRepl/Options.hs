{-# LANGUAGE OverloadedStrings #-}

module LambdaRepl.Options
  ( runApp,
    evalCmd,
    tcCmd,
    helpCmd,
    printEnvCmd,
    updateEnvTraceCmd,
    updateEnvStrategyCmd,
    listStrategyCmd,
    listPreludeCmd
    )
where

import Data.Text.Prettyprint.Doc
import LambdaRepl.Types
import Language.Core
import RIO hiding (trace)
import qualified RIO.List as List
import RIO.Process
import qualified RIO.Text as Text
import System.Console.Haskeline hiding (display)
import System.Environment

runApp :: MonadIO m => RIO ReplEnv a -> m a
runApp m =
  liftIO $ do
    verbose <- isJust <$> lookupEnv "RIO_VERBOSE"
    lo <- logOptionsHandle stderr verbose
    pc <- mkDefaultProcessContext
    strategy <- newIORef NormalOrder
    isTrace <- newIORef False
    withLogFunc lo $ \lf ->
      let app = ReplEnv
            { appLogFunc = lf,
              appProcessContext = pc,
              appStrategy = strategy,
              appIsTrace = isTrace
              }
       in runRIO app m

evalCmd :: Pretty term => ParseFunc term -> EvalFunc term -> TraceFunc term -> Text -> LambdaREPL
evalCmd parser evaluator tracer input =
  lift $ ask >>= \ReplEnv {..} -> do
    strategy <- readIORef appStrategy
    isTrace <- readIORef appIsTrace
    case parser (Text.unpack input) of
      Left err -> logError $ display $ Text.pack err
      Right term ->
        if isTrace
          then mapM_ (logInfo . displayRender) $ tracer strategy term
          else logInfo $ displayRender $ evaluator strategy term

tcCmd :: Pretty t => ParseFunc term -> (term -> t) -> Text -> LambdaREPL
tcCmd parser checker input =
  lift $ case Text.stripPrefix ":t " input of
    Nothing -> do
      logInfo "コマンドがの書式が間違っています。"
      return ()
    Just input' -> case parser (Text.unpack input') of
      Left err -> logError $ display $ Text.pack err
      Right term -> logInfo $ display $ Text.pack $ render $ checker term

helpCmd :: LambdaREPL
helpCmd = lift (mapM_ (logInfo . display) $ "available commands" : commands)

commands :: [Text]
commands =
  [ "  :set trace               -- トレースの有効化 (簡約の途中経過も含めて表示)",
    "  :set strategy <Strategy> -- 評価戦略の設定",
    "  :unset trace             -- トレースの無効化",
    "  :list strategy           -- 評価戦略の一覧を表示",
    "  :list prelude            -- prelude 関数の一覧を表示",
    "  :env                     -- 現在の設定内容を表示",
    "  :help                    -- ヘルプ",
    "  :q                       -- 終了"
    ]

printEnvCmd :: LambdaREPL
printEnvCmd =
  lift $ ask >>= \ReplEnv {..} -> do
    strategy <- readIORef appStrategy
    isTrace <- readIORef appIsTrace
    let msg =
          Text.unlines
            [ "strategy: " <> tshow strategy,
              "isTrace: " <> tshow isTrace
              ]
    logInfo $ display msg

updateEnvTraceCmd :: Bool -> LambdaREPL
updateEnvTraceCmd newIsTrace = do
  lift $ ask >>= \ReplEnv {..} -> writeIORef appIsTrace newIsTrace
  printEnvCmd

updateEnvStrategyCmd :: Text -> LambdaREPL
updateEnvStrategyCmd input = do
  lift $ case readLastInput input of
    Nothing -> logError "Invalid strategy"
    Just newStrategy ->
      ask >>= \ReplEnv {..} ->
        writeIORef appStrategy newStrategy
  printEnvCmd

readLastInput :: Read a => Text -> Maybe a
readLastInput input = do
  lastInput <- List.lastMaybe $ Text.words input
  readMaybe $ Text.unpack lastInput

listStrategyCmd :: LambdaREPL
listStrategyCmd = mapM_ (outputStrLn . show) strategies

listPreludeCmd :: Pretty lang => Prelude lang -> LambdaREPL
listPreludeCmd = outputStrLn . renderPrelude
