{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Examples
  ( example1
  , example2
  , example3
  , example4
  , example5
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

-- | (λx.x) x
example4 :: Term
example4 = TmApp (TmLam "x" "x") "x"

-- | λz. λx. λy. x (y z)
example5 :: Term
example5 = TmLam "z" (TmLam "x" (TmLam "y" (TmApp "x" (TmApp "y" "z"))))