{-# LANGUAGE OverloadedStrings #-}
module Language.Extensible.UntypedLambda.Prelude
  ( id
  ) where

import Prelude hiding (id)

import           Language.Extensible.UntypedLambda.Types

import Data.Extensible
import Control.Lens    (( # ))

-- | λx. x
id :: Term
id = Term $ #lambda # ("x", "x")