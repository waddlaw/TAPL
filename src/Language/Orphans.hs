{-# LANGUAGE NoImplicitPrelude #-}
{-# GHC_OPTIONS -f-no-warn-orphans #-}
module Language.Orphans () where

import           RIO

import           System.Console.Haskeline hiding (display)

instance MonadException (RIO env) where
  controlIO f = RIO $ controlIO $ \(RunIO run) -> let
    run' = RunIO (fmap RIO . run . unRIO)
    in (unRIO Control.Applicative.<$> f run')
