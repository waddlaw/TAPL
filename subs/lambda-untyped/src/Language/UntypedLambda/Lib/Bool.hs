{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda.Lib.Bool
  ( -- * value
    tru,
    fls,
    test,
    and,

    -- * 演習 5.2.1
    or,
    not,

    -- * helper
    mkTest,
    mkAnd,
    mkOr,
    mkNot,
  )
where

import Language.UntypedLambda.Lib.Util
import Language.UntypedLambda.Types
import Prelude hiding ((.))

-- | λt. λf. t
tru :: UntypedLambda
tru = λ "t" . λ "f" . "t"

-- | λt. λf. f
fls :: UntypedLambda
fls = λ "t" . λ "f" . "f"

-- | λl. λm. λn. l m n
test :: UntypedLambda
test = λ "l" . λ "m" . λ "n" . "l" @@ "m" @@ "n"

-- | λb. λc. b c fls
and :: UntypedLambda
and = λ "b" . λ "c" . "b" @@ "c" @@ fls

-- | λb. λc. b tru c
or :: UntypedLambda
or = λ "b" . λ "c" . "b" @@ tru @@ "c"

-- | λb. b fls tru
not :: UntypedLambda
not = λ "b" . "b" @@ fls @@ tru

mkTest :: UntypedLambda -> UntypedLambda -> UntypedLambda -> UntypedLambda
mkTest b t1 t2 = test @@ b @@ t1 @@ t2

mkAnd :: UntypedLambda -> UntypedLambda -> UntypedLambda
mkAnd t1 t2 = and @@ t1 @@ t2

mkOr :: UntypedLambda -> UntypedLambda -> UntypedLambda
mkOr t1 t2 = or @@ t1 @@ t2

mkNot :: UntypedLambda -> UntypedLambda
mkNot t = not @@ t
