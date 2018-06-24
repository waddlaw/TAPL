{-# LANGUAGE OverloadedStrings #-}
module Extensible.Language.UntypedLambda.Prelude
  ( id
  , tru, fls, test, and, or, not
  , pair, fst, snd
  , c
  ) where

import Prelude hiding (id, and, or, not, fst, snd)

import           Extensible.Language.UntypedLambda.Types

import Control.Lens    (( # ))

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