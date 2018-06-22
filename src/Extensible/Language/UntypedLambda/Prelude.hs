{-# LANGUAGE OverloadedStrings #-}
module Extensible.Language.UntypedLambda.Prelude
  ( id
  ) where

import Prelude hiding (id)

import           Extensible.Language.UntypedLambda.Types

import Control.Lens    (( # ))

-- | λx. x
id :: Term
id = Term $ #lambda # ("x", "x")