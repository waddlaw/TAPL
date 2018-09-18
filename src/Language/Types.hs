{-# LANGUAGE NoImplicitPrelude #-}
module Language.Types
  ( ReplEnv (..)
  , ParseFunc
  , EvalFunc
  ) where

import           RIO
import           RIO.Process

import           Language.UntypedLambda.Types

type ParseFunc term = Text -> Either String term
type EvalFunc term = Strategy -> term -> term

data ReplEnv = ReplEnv
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appStrategy       :: !Strategy
  , appIsTrace        :: !Bool
  }

instance HasLogFunc ReplEnv where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext ReplEnv where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
