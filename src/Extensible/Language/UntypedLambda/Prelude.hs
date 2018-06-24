{-# LANGUAGE OverloadedStrings #-}
module Extensible.Language.UntypedLambda.Prelude
  ( prelude
  , id
  , tru, fls, test, and, or, not
  , pair, fst, snd
  , c, scc, plus, times, iszro, prd
  , scc2, times2, times3, power1, power2, subtract1, equal
  ) where

import           Prelude                                 hiding (and, fst, id,
                                                          not, or, snd)

import           Extensible.Language.UntypedLambda.Types

import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Text                               (Text)

prelude :: Map Text Term
prelude = Map.fromList
  [ ("id", id), ("tru", tru), ("fls", fls), ("test", test), ("and", and), ("or", or), ("not", not)
  , ("pair", pair), ("fst", fst), ("snd", snd)
  , ("scc", scc), ("plus", plus), ("times", times), ("power", power1), ("iszro", iszro), ("prd", prd), ("subtract", subtract1), ("equal", equal)
  -- , ("nil", nil), ("cons", cons), ("isnil", isnil), ("head", head), ("tail", tail)
  ]

-- | λx. x
id :: Term
id = lambda "x" "x"

-- | λt. λf. t
tru :: Term
tru = lambda "t" $ lambda "f" "t"

-- | λt. λf. f
fls :: Term
fls = lambda "t" $ lambda "f" "f"

-- | λl. λm. λn. l m n
test :: Term
test = lambda "l" $ lambda "m" $ lambda "n" $ app (app "l" "m") "n"

-- | λb. λc. b c fls
and :: Term
and = lambda "b" $ lambda "c" $ app (app "b" "c") fls

-- | λb. λc. b tru c
or :: Term
or = lambda "b" $ lambda "c" $ app (app "b" tru) "c"

-- | λb. b fls tru
not :: Term
not = lambda "b" $ app (app "b" fls) tru

-- | λf. λs. λb. b f s
pair :: Term
pair = lambda "f" $ lambda "s" $ lambda "b" $ app (app "b" "f") "s"

-- | λp. p tru
fst :: Term
fst = lambda "p" $ app "p" tru

-- | λp. p fls
snd :: Term
snd = lambda "p" $ app "p" fls

-- |
-- c0 = λs. λz. z
--
-- c1 = λs. λz. s z
--
-- c2 = λs. λz. s (s z)
--
-- c3 = λs. λz. s (s (s z))
c :: Int -> Term
c n = lambda "s" $ lambda "z" body
  where
    body = foldr app "z" $ replicate n "s"

-- | λn. λs. λz. s (n s z)
scc :: Term
scc = lambda "n" $ lambda "s" $ lambda "z" $ app "s" (app (app "n" "s") "z")

-- | λn. λs. λz. n s (s z)
scc2 :: Term
scc2 = lambda "n" $ lambda "s" $ lambda "z" $ app (app "n" "s") (app "s" "z")

-- | λm. λn. λs. λz. m s (n s z)
plus :: Term
plus = lambda "m" $ lambda "n" $ lambda "s" $ lambda "z" $ app (app "m" "s") (app (app "n" "s") "z")

-- | λm. λn. m (plus n) c0
times :: Term
times = lambda "m" $ lambda "n" $ app (app "m" (app plus "n")) (c 0)

-- | λm. λn. λs. λz. m (n s) z
times2 :: Term
times2 = lambda "m" $ lambda "n" $ lambda "s" $ lambda "z" $ app (app "m" (app "n" "s")) "z"

-- | λm. λn. λs. m (n s)
times3 :: Term
times3 = lambda "m" $ lambda "n" $ lambda "s" $ app "m" (app "n" "s")

-- | λn. λm. m (times n) c1
--
-- n^m
power1 :: Term
power1 = lambda "n" $ lambda "m" $ app (app "m" (app times "n")) (c 1)

-- | λn. λm. m n
--
-- m^n
power2 :: Term
power2 = lambda "n" $ lambda "m" $ app "m" "n"

-- | λm. m (λx. fls) tru
iszro :: Term
iszro = lambda "m" $ app (app "m" (lambda "x" fls)) tru

-- | pair c0 c0
zz :: Term
zz = app (app pair (c 0)) (c 0)

-- | λp. pair (snd p) (plus c1 (snd p))
ss :: Term
ss = lambda "p" $ app (app pair (app snd "p")) (app (app plus (c 1)) (app snd "p"))

-- | λm. fst (m ss zz)
prd :: Term
prd = lambda "m" $ app fst (app (app "m" ss) zz)

-- | λm. λn. n prd m
subtract1 :: Term
subtract1 = lambda "m" $ lambda "n" $ app (app "n" prd) "m"

-- | λm. λn. and (iszro (m prd n)) (iszro (n prd m))
equal :: Term
equal = lambda "m" $ lambda "n" $ app (app and (app iszro l)) (app iszro r)
  where
    l = app (app "m" prd) "n"
    r = app (app "n" prd) "m"

