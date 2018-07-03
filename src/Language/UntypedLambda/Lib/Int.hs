{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.Int
  ( int
  , succI
  , succNI
  , plusI
  ) where

import           Prelude                        hiding (fst, snd, not, and)

import           Language.UntypedLambda.Prelude
import           Language.UntypedLambda.Types

-- | type Int = (Bool, Nat)
-- 1  = (True, 1)
-- -1 = (False, 1)
-- -0 = (False, 0)
-- +0 = (True, 0)
int :: Int -> UntypedLambda
int n
  | n < 0     = mkPair fls (c $ abs n)
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

-- | λf. λp. test (iszro (fst p)) (λx. snd p) (λx. succI (f (pair (prd (fst p)) (snd p)))) c0
succNI :: UntypedLambda
succNI = mkFix "p" match base rec
  where
    match = iszro @@ (fst @@ "p")
    base = λ "x" $ snd @@ "p"
    rec  = λ "x" $ succI @@ ("f" @@ mkPair (prd @@ (fst @@ "p")) (snd @@ "p"))

-- | if isZero i
--   then (True, 1)
--   else if isNegative i
--        then (fst i, prd (snd i))
--        else (fst i, scc (snd i))
--
-- λi. test (isZeroI i) (pair tru c1) (test (isPositiveI i) (pair (fst i) (scc (snd i))) (pair (fst i) (prd (snd i))))
succI :: UntypedLambda
succI = λ "i" $ mkTest c1 t1 $ mkTest c2 t2 t3
  where
    c1 = isZeroI @@ "i"
    c2 = isPositiveI @@ "i"
    t1 = mkPair tru (c 1)
    t2 = mkPair (fst @@ "i") (scc @@ (snd @@ "i"))
    t3 = mkPair (fst @@ "i") (prd @@ (snd @@ "i"))

-- | λi. iszro (snd i)
isZeroI :: UntypedLambda
isZeroI = λ "i" $ iszro @@ (snd @@ "i")

-- | λi. fst i
isPositiveI :: UntypedLambda
isPositiveI = λ "i" $ fst @@ "i"

