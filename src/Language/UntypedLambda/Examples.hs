{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Examples
  ( example1
  ) where

import           Language.UntypedLambda

example1 :: Term
example1 = TmLam "x" (TmLam "y" (TmApp (TmApp x y) x))

x, y :: Term
x = TmVar "x"
y = TmVar "y"
