{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
  ) where

import           Prelude                hiding (id)

import           Language.UntypedLambda

-- | λx. x
id :: Term
id = TmLam "x" "x"
