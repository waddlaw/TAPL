{-# LANGUAGE OverloadedStrings #-}
module Extensible.Language.UntypedLambda.Prelude
  ( id
  , tru, fls, test, and, or, not
  ) where

import Prelude hiding (id, and, or, not)

import           Extensible.Language.UntypedLambda.Types

import Control.Lens    (( # ))

-- | λx. x
id :: Term
id = Term $ #lambda # ("x", "x")

-- | λt. λf. t
tru :: Term
tru = Term $ #lambda # ("t", t)
  where t = Term $ #lambda # ("f", "t")

-- | λt. λf. f
fls :: Term
fls = Term $ #lambda # ("t", t)
  where t = Term $ #lambda # ("f", "f")

-- | λl. λm. λn. l m n
test :: Term
test = Term $ #lambda # ("l", t1)
  where
    t1 = Term $ #lambda # ("m", t2)
    t2 = Term $ #lambda # ("n", t3)
    t3 = Term $ #app    # (t4, "n")
    t4 = Term $ #app    # ("l", "m")

-- | λb. λc. b c fls
and :: Term
and = Term $ #lambda # ("b", t1)
  where
    t1 = Term $ #lambda # ("c", t2)
    t2 = Term $ #app    # (t3, fls)
    t3 = Term $ #app    # ("b", "c")

-- | λb. λc. b tru c
or :: Term
or = Term $ #lambda # ("b", t1)
  where
    t1 = Term $ #lambda # ("c", t2)
    t2 = Term $ #app    # (t3, "c")
    t3 = Term $ #app    # ("b", tru)

-- | λb. b fls tru
not :: Term
not = Term $ #lambda # ("b", t1)
  where
    t1 = Term $ #app # (t2, tru)
    t2 = Term $ #app # ("b", fls)