{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
  , prelude
  -- * Church ブール値
  , tru
  , fls
  , test
  , and
  -- ** 演習 5.2.1
  , or
  , not
  -- * 二つ組
  , pair
  , fst
  , snd
  -- * Church 数
  , c
  , scc
  , plus
  , times
  , iszro
  , prd
  -- ** 演習5.2.2
  , scc2
  -- ** 演習5.2.3
  , times2
  , times3
  -- ** 演習5.2.4
  , power1
  , power2
  -- ** 演習5.2.5
  , subtract1
  -- ** 演習5.2.7
  , equal
  -- ** 演習5.2.8
  , nil
  , cons
  , isnil
  , head
  , tail
  -- * fix
  , fix
  -- ** 演習5.2.9
  , factorial
  -- ** 演習5.2.11
  , sumlist
  , sumlist'
  -- extra
  , int
  , succI
  , succNI
  , plusI
  ) where

import           Prelude                      hiding (and, fst, head, id, not,
                                               or, snd, tail)

import           Language.UntypedLambda.Types

import           Data.Map                     (Map)
import qualified Data.Map                     as Map (fromList)
import           Data.Text                    (Text)

prelude :: Map Text UntypedLambda
prelude = Map.fromList
  [ ("id", id), ("tru", tru), ("fls", fls), ("test", test), ("and", and), ("or", or), ("not", not)
  , ("pair", pair), ("fst", fst), ("snd", snd)
  , ("scc", scc), ("plus", plus), ("times", times), ("power", power1), ("iszro", iszro), ("prd", prd), ("subtract", subtract1), ("equal", equal)
  , ("nil", nil), ("cons", cons), ("isnil", isnil), ("head", head), ("tail", tail)
  ]

-- | λx. x
id :: UntypedLambda
id = TmLam "x" "x"

-- | λt. λf. t
tru :: UntypedLambda
tru = TmLam "t" (TmLam "f" "t")

-- | λt. λf. f
fls :: UntypedLambda
fls = TmLam "t" (TmLam "f" "f")

-- | λl. λm. λn. l m n
test :: UntypedLambda
test = TmLam "l" (TmLam "m" (TmLam "n" (TmApp (TmApp "l" "m") "n")))

-- | λb. λc. b c fls
and :: UntypedLambda
and = TmLam "b" (TmLam "c" (TmApp (TmApp "b" "c") fls))

-- | λb. λc. b tru c
or :: UntypedLambda
or = TmLam "b" (TmLam "c" (TmApp (TmApp "b" tru) "c"))

-- | λb. b fls tru
not :: UntypedLambda
not = TmLam "b" (TmApp (TmApp "b" fls) tru)

-- | λf. λs. λb. b f s
pair :: UntypedLambda
pair = TmLam "f" (TmLam "s" (TmLam "b" (TmApp (TmApp "b" "f") "s")))

-- | λp. p tru
fst :: UntypedLambda
fst = TmLam "p" (TmApp "p" tru)

-- | λp. p fls
snd :: UntypedLambda
snd = TmLam "p" (TmApp "p" fls)

-- |
-- c0 = λs. λz. z
--
-- c1 = λs. λz. s z
--
-- c2 = λs. λz. s (s z)
--
-- c3 = λs. λz. s (s (s z))
c :: Int -> UntypedLambda
c n = TmLam "s" (TmLam "z" body)
  where
    body = foldr TmApp "z" $ replicate n "s"

-- | λn. λs. λz. s (n s z)
scc :: UntypedLambda
scc = TmLam "n" (TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp "n" "s") "z"))))

-- | λn. λs. λz. n s (s z)
scc2 :: UntypedLambda
scc2 = TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "n" "s") (TmApp "s" "z"))))

-- | λm. λn. λs. λz. m s (n s z)
plus :: UntypedLambda
plus = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" "s") (TmApp (TmApp "n" "s") "z")))))

-- | λm. λn. m (plus n) c0
times :: UntypedLambda
times = TmLam "m" (TmLam "n" (TmApp (TmApp "m" (TmApp plus "n")) (c 0)))

-- | λm. λn. λs. λz. m (n s) z
times2 :: UntypedLambda
times2 = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" (TmApp "n" "s")) "z"))))

-- | λm. λn. λs. m (n s)
times3 :: UntypedLambda
times3 = TmLam "m" (TmLam "n" (TmLam "s" (TmApp "m" (TmApp "n" "s"))))

-- | λn. λm. m (times n) c1
--
-- n^m
power1 :: UntypedLambda
power1 = TmLam "n" (TmLam "m" (TmApp (TmApp "m" (TmApp times "n")) (c 1)))

-- | λn. λm. m n
--
-- m^n
power2 :: UntypedLambda
power2 = TmLam "n" (TmLam "m" (TmApp "m" "n"))

-- | λm. m (λx. fls) tru
iszro :: UntypedLambda
iszro = TmLam "m" (TmApp (TmApp "m" (TmLam "x" fls)) tru)

-- | pair c0 c0
zz :: UntypedLambda
zz = TmApp (TmApp pair (c 0)) (c 0)

-- | λp. pair (snd p) (plus c1 (snd p))
ss :: UntypedLambda
ss = TmLam "p" (TmApp (TmApp pair (TmApp snd "p")) (TmApp (TmApp plus (c 1)) (TmApp snd "p")))

-- | λm. fst (m ss zz)
prd :: UntypedLambda
prd = TmLam "m" (TmApp fst (TmApp (TmApp "m" ss) zz))

-- | λm. λn. n prd m
subtract1 :: UntypedLambda
subtract1 = TmLam "m" (TmLam "n" (TmApp (TmApp "n" prd) "m"))

-- | λm. λn. and (iszro (m prd n)) (iszro (n prd m))
equal :: UntypedLambda
equal = TmLam "m" (TmLam "n" (TmApp (TmApp and (TmApp iszro l)) (TmApp iszro r)))
  where
    l = TmApp (TmApp "m" prd) "n"
    r = TmApp (TmApp "n" prd) "m"

-- | λc. λn. n
nil :: UntypedLambda
nil = TmLam "c" (TmLam "n" "n")

-- | λh. λt. λc. λn. c h (t c n)
cons :: UntypedLambda
cons = TmLam "h" (TmLam "t" (TmLam "c" (TmLam "n" (TmApp (TmApp "c" "h") (TmApp (TmApp "t" "c") "n")))))

-- | λl. l (λh. λt. fls) tru
isnil :: UntypedLambda
isnil = TmLam "l" (TmApp (TmApp "l" (TmLam "h" (TmLam "t" fls))) tru)

-- | λl. l (λh. λt. h) l
head :: UntypedLambda
head = TmLam "l" (TmApp (TmApp "l" (TmLam "h" (TmLam "t" "h"))) "l")

-- | pair nil nil
nn :: UntypedLambda
nn = TmApp (TmApp pair nil) nil

-- | λh. λp. pair (snd p) (cons h (snd p))
cc :: UntypedLambda
cc = TmLam "h" (TmLam "p" (TmApp (TmApp pair (TmApp snd "p")) (TmApp (TmApp cons "h") (TmApp snd "p"))))

-- | λl. fst (l cc nn)
tail :: UntypedLambda
tail = TmLam "l" (TmApp fst (TmApp (TmApp "l" cc) nn))

-- | λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
fix :: UntypedLambda
fix = TmLam "f" $ TmApp t t
  where
    t = TmLam "x" $ TmApp "f" (TmLam "y" (TmApp (TmApp "x" "x") "y"))

factorial :: UntypedLambda
factorial = TmApp fix ff
  where
    ff = TmLam "f" $ TmLam "n" $ TmApp (TmApp (TmApp (TmApp test (TmApp iszro "n")) (TmLam "x" $ c 1)) t) (c 0)
    t  = TmLam "x" $ TmApp (TmApp times "n") (TmApp "f" (TmApp prd "n"))

sumlist :: UntypedLambda
sumlist = TmApp fix ff
  where
    ff = TmLam "f" $ TmLam "l" $ TmApp (TmApp (TmApp (TmApp test (TmApp isnil "l")) (TmLam "x" $ c 0)) t) (c 0)
    t  = TmLam "x" $ TmApp (TmApp plus (TmApp head "l")) (TmApp "f" (TmApp tail "l"))

sumlist' :: UntypedLambda
sumlist' = TmLam "l" $ TmApp (TmApp "l" plus) (c 0)

int :: Int -> UntypedLambda
int n
  | n < 0     = TmApp (TmApp pair fls) (c $ abs n)
  | otherwise = TmApp (TmApp pair tru) (c n)

-- |
-- plusI i1 i2
--   | i1 >= 0 && i2 >= 0 = succNI (snd i1) i2
plusI :: UntypedLambda
plusI = TmLam "i1" $ TmLam "i2" $ TmApp (TmApp (TmApp test isPP) pp) t2
  where
    isPP = TmApp (TmApp and c1l) c1r
    c1l  = TmApp fst "i1"
    c1r  = TmApp fst "i2"
    -- positive + positive
    pp = TmApp succNI (TmApp (TmApp pair (TmApp snd "i1")) "i2")
    t2 = TmApp (TmApp (TmApp test isNN) nn) t3
    isNN = TmApp (TmApp and c2l) c2r
    c2l  = TmApp not (TmApp fst "i1")
    c2r  = TmApp not (TmApp fst "i2")
    -- negative + negative
    nn   = TmApp (TmApp pair fls) (TmApp snd (TmApp succNI (TmApp (TmApp pair (TmApp snd "i1")) (TmApp (TmApp pair tru) (TmApp snd "i2")))))
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