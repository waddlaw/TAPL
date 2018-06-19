{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
  -- Church ブール値
  , tru
  , fls
  , test
  ) where

import           Prelude                hiding (id)

import           Language.UntypedLambda

-- | λx. x
id :: Term
id = TmLam "x" "x"

-- | λt. λf. t
tru :: Term
tru = TmLam "t" (TmLam "f" "t")

-- | λt. λf. f
fls :: Term
fls = TmLam "t" (TmLam "f" "f")

-- | λl. λm. λn. l m n
test :: Term
test = TmLam "l" (TmLam "m" (TmLam "n" (TmApp (TmApp "l" "m") "n")))