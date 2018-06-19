{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Prelude
  ( id
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
  -- ** 演習5.2.6
  , equal
  ) where

import           Prelude                      hiding (and, fst, id, not, or,
                                               snd)

import           Language.UntypedLambda.Types

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

-- | λb. λc. b c fls
and :: Term
and = TmLam "b" (TmLam "c" (TmApp (TmApp "b" "c") fls))

-- | λb. λc. b tru c
or :: Term
or = TmLam "b" (TmLam "c" (TmApp (TmApp "b" tru) "c"))

-- | λb. b fls tru
not :: Term
not = TmLam "b" (TmApp (TmApp "b" fls) tru)

-- | λf. λs. λb. b f s
pair :: Term
pair = TmLam "f" (TmLam "s" (TmLam "b" (TmApp (TmApp "b" "f") "s")))

-- | λp. p tru
fst :: Term
fst = TmLam "p" (TmApp "p" tru)

-- | λp. p fls
snd :: Term
snd = TmLam "p" (TmApp "p" fls)

-- |
-- c0 = λs. λz. z
--
-- c1 = λs. λz. s z
--
-- c2 = λs. λz. s (s z)
--
-- c3 = λs. λz. s (s (s z))
c :: Int -> Term
c n = TmLam "s" (TmLam "z" body)
  where
    body = foldr TmApp "z" $ replicate n "s"

-- | λn. λs. λz. s (n s z)
scc :: Term
scc = TmLam "n" (TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp "n" "s") "z"))))

-- | λn. λs. λz. n s (s z)
scc2 :: Term
scc2 = TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "n" "s") (TmApp "s" "z"))))

-- | λm. λn. λs. λz. m s (n s z)
plus :: Term
plus = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" "s") (TmApp (TmApp "n" "s") "z")))))

-- | λm. λn. m (plus n) c0
times :: Term
times = TmLam "m" (TmLam "n" (TmApp (TmApp "m" (TmApp plus "n")) (c 0)))

-- | λm. λn. λs. λz. m (n s) z
times2 :: Term
times2 = TmLam "m" (TmLam "n" (TmLam "s" (TmLam "z" (TmApp (TmApp "m" (TmApp "n" "s")) "z"))))

-- | λm. λn. λs. m (n s)
times3 :: Term
times3 = TmLam "m" (TmLam "n" (TmLam "s" (TmApp "m" (TmApp "n" "s"))))

-- | λn. λm. m (times n) c1
-- n^m
power1 :: Term
power1 = TmLam "n" (TmLam "m" (TmApp (TmApp "m" (TmApp times "n")) (c 1)))

-- | λn. λm. m n
-- m^n
power2 :: Term
power2 = TmLam "n" (TmLam "m" (TmApp "m" "n"))

-- | λm. m (λx. fls) tru
iszro :: Term
iszro = TmLam "m" (TmApp (TmApp "m" (TmLam "x" fls)) tru)

-- | pair c0 c0
zz :: Term
zz = TmApp (TmApp pair (c 0)) (c 0)

-- | λp. pair (snd p) (plus c1 (snd p))
ss :: Term
ss = TmLam "p" (TmApp (TmApp pair (TmApp snd "p")) (TmApp (TmApp plus (c 1)) (TmApp snd "p")))

-- | λm. fst (m ss zz)
prd :: Term
prd = TmLam "m" (TmApp fst (TmApp (TmApp "m" ss) zz))

-- | λm. λn. n prd m
subtract1 :: Term
subtract1 = TmLam "m" (TmLam "n" (TmApp (TmApp "n" prd) "m"))

-- | λm. λn. and (iszro (m prd n)) (iszro (n prd m))
equal :: Term
equal = TmLam "m" (TmLam "n" (TmApp (TmApp and (TmApp iszro l)) (TmApp iszro r)))
  where
    l = TmApp (TmApp "m" prd) "n"
    r = TmApp (TmApp "n" prd) "m"
