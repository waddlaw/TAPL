{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.UntypedLambda.Lib.Int
  ( int
  , succI
  , succNI
  , plusI
  , isZeroI
  , isAbsOneI
  ) where

import RIO hiding (and, not, fst, snd)

import           Language.UntypedLambda.Lib.Base
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Lib.Pair
import           Language.UntypedLambda.Types

-- | int = λb. λs. λz. z
--       | λb. λs. λz. pair b (s z)
-- 0  = λs. λz. z
-- -1 = λs. λz. pair fls (s z)
-- +1 = λs. λz. pair tru (s z)
int :: Int -> UntypedLambda
int n
  | n == 0 = λ "b" $ c 0
  | n <  0 = mkPair fls (c $ abs n)
  | otherwise = mkPair tru (c n)

plusI :: UntypedLambda
plusI = λ "n" $ λ "m" $ mkTest isPP pp $ mkTest isNN nn' t3
  where
    -- positive + positive
    isPP = and @@ (fst @@ "n") @@ (fst @@ "m")
    pp   = succNI @@ mkPair (snd @@ "n") "m"

    -- negative + negative
    isNN = and @@ (not @@ (fst @@ "n")) @@ (not @@ (fst @@ "m"))
    nn'  = mkPair fls $ snd @@ (succNI @@ mkPair (snd @@ "n") (mkPair tru (snd @@ "m")))
    -- cmp 作るの疲れたから left: positive, right: negative とする
    t3 = succNI @@ mkPair (snd @@ "n") "m"

-- |
-- match = iszro (fst p)
-- base  = snd p
-- rec   = succI (f (mkPair (prd (fst p)) (snd p))
succNI :: UntypedLambda
succNI = mkFix "p" match base rec
  where
    match = iszro @@ (fst @@ "p")
    base = snd @@ "p"
    rec  = succI @@ ("f" @@ mkPair (prd @@ (fst @@ "p")) (snd @@ "p"))

-- | if isZero i
--   then (True, 1)
--   else if isNegative i
--        then (fst i, prd (snd i))
--        else (fst i, scc (snd i))
--
-- λi. test (isZeroI i) (pair tru c1) (test (isPositiveI i) (pair (fst i) (scc (snd i))) (pair (fst i) (prd (snd i))))
succI :: UntypedLambda
succI = λ "i" $ mkTest c1 t1 $ mkTest c2 t2 $ mkTest c3 t3 t4
  where
    c1 = isZeroI @@ "i"
    c2 = isPositiveI @@ "i"
    c3 = isAbsOneI @@ "i"
    t1 = mkPair tru (c 1)
    t2 = mkPair (fst @@ "i") (scc @@ (snd @@ "i"))
    t3 = int 0
    t4 = mkPair (fst @@ "i") (prd @@ (snd @@ "i"))

-- | λi. iszro (snd i)
isZeroI :: UntypedLambda
isZeroI = λ "i" $ iszro @@ (snd @@ "i")

-- | λi. isone (snd i)
isAbsOneI :: UntypedLambda
isAbsOneI = λ "i" $ and @@ c1 @@ c2
  where
    c1 = not @@ (isZeroI @@ "i")
    c2 = isone @@ (snd @@ "i")

-- | λi. fst i
isPositiveI :: UntypedLambda
isPositiveI = λ "i" $ fst @@ "i"
