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
  | n < 0     = TmApp (TmApp pair fls) (c $ abs n)
  | otherwise = TmApp (TmApp pair tru) (c n)

plusI :: UntypedLambda
plusI = TmLam "i1" $ TmLam "i2" $ TmApp (TmApp (TmApp test isPP) pp) t2
  where
    isPP = TmApp (TmApp and c1l) c1r
    c1l  = TmApp fst "i1"
    c1r  = TmApp fst "i2"
    -- positive + positive
    pp = TmApp succNI (TmApp (TmApp pair (TmApp snd "i1")) "i2")
    t2 = TmApp (TmApp (TmApp test isNN) nn') t3
    isNN = TmApp (TmApp and c2l) c2r
    c2l  = TmApp not (TmApp fst "i1")
    c2r  = TmApp not (TmApp fst "i2")
    -- negative + negative
    nn'  = TmApp (TmApp pair fls) (TmApp snd (TmApp succNI (TmApp (TmApp pair (TmApp snd "i1")) (TmApp (TmApp pair tru) (TmApp snd "i2")))))
    -- cmp 作るの疲れたから left: positive, right: negative とする
    t3 = TmApp succNI (TmApp (TmApp pair (TmApp snd "i1")) "i2")

succNI :: UntypedLambda
succNI = TmApp fix ff
  where
    ff = TmLam "f" $ TmLam "p" $ TmApp (TmApp (TmApp (TmApp test (TmApp iszro (TmApp fst "p"))) (TmLam "x" $ TmApp snd "p")) t) (c 0)
    t  = TmLam "x" $ TmApp succI (TmApp "f" (TmApp (TmApp pair (TmApp prd (TmApp fst "p"))) (TmApp snd "p")))

-- | if isZero i
--   then (True, 1)
--   else if isNegative i
--        then (fst i, prd (snd i))
--        else (fst i, scc (snd i))
succI :: UntypedLambda
succI = TmLam "i" $ TmApp (TmApp (TmApp test c1) t1) t2
  where
    c1 = TmApp isZeroI "i"
    t1 = TmApp (TmApp pair tru) (c 1)
    t2 = TmApp (TmApp (TmApp test c2) t3) t4
    c2 = TmApp isPositiveI "i"
    t3 = TmApp (TmApp pair (TmApp fst "i")) (TmApp scc (TmApp snd "i"))
    t4 = TmApp (TmApp pair (TmApp fst "i")) (TmApp prd (TmApp snd "i"))

isZeroI :: UntypedLambda
isZeroI = TmLam "i" $ TmApp iszro (TmApp snd "i")

isPositiveI :: UntypedLambda
isPositiveI = TmLam "i" $ TmApp fst "i"
