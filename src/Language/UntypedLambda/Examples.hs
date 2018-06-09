{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Examples
  ( example1
  , example2
  , example3
  ) where

import           Prelude                        hiding (id)

import           Language.UntypedLambda
import           Language.UntypedLambda.Prelude

-- | s t u
example1 :: Term
example1 = TmApp (TmApp "s" "t") "u"

-- | λx. (λy. ((x y) x))
example2 :: Term
example2 = TmLam "x" (TmLam "y" (TmApp (TmApp "x" "y") "x"))

-- | (λx.x) ((λx.x) (λz. (λx.x) z))
example3 :: Term
example3 = TmApp id (TmApp id (TmLam "z" (TmApp id "z")))
