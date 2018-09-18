{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.List
  ( nil
  , cons
  , isnil
  , head
  , tail
  -- ** 演習5.2.11
  , sumlist
  , sumlist'
  ) where

import           RIO                               hiding (fst, snd)

import           Language.UntypedLambda.Lib.Base
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Lib.Pair
import           Language.UntypedLambda.Types

-- | λc. λn. n
nil :: UntypedLambda
nil = λ "c" $ λ "n" "n"

-- | λh. λt. λc. λn. c h (t c n)
cons :: UntypedLambda
cons = λ "h" $ λ "t" $ λ "c" $ λ "n" $ "c" @@ "h" @@ ("t" @@ "c" @@ "n")

-- | λl. l (λh. λt. fls) tru
isnil :: UntypedLambda
isnil = λ "l" $ "l" @@ λ "h" (λ "t" fls) @@ tru

-- | λl. l (λh. λt. h) l
head :: UntypedLambda
head = λ "l" $ "l" @@ λ "h" (λ "t" "h") @@ "l"

-- | pair nil nil
nn :: UntypedLambda
nn = mkPair nil nil

-- | λh. λp. pair (snd p) (cons h (snd p))
cc :: UntypedLambda
cc = λ "h" $ λ "p" $ mkPair (snd @@ "p") (cons @@ "h" @@ (snd @@ "p"))

-- | λl. fst (l cc nn)
tail :: UntypedLambda
tail = λ "l" $ fst @@ ("l" @@ cc @@ nn)

-- |
--   match = isnil l
--   base  = c0
--   rec   = plus (head l) (f (tail l))
sumlist :: UntypedLambda
sumlist = mkFix "l" match base rec
  where
    match = isnil @@ "l"
    base  = c 0
    rec   = plus @@ (head @@ "l") @@ ("f" @@ (tail @@ "l"))

-- | λl. l plus c0
sumlist' :: UntypedLambda
sumlist' = λ "l" $ "l" @@ plus @@ c 0
