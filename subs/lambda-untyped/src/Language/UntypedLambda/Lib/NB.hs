{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda.Lib.NB
  ( realbool,
    churchbool,
    realeq,
    realnat,

    -- * Exercise 5.2.10
    churchnat,
  )
where

import Language.UntypedLambda.Lib.Base
import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Church
import Language.UntypedLambda.Lib.Util
import Language.UntypedLambda.Types
import Prelude hiding ((.))

-- | Convert Church Boolean to Primitive Boolean
-- λb. b true false
realbool :: UntypedLambda
realbool = λ "b" . "b" @@ "true" @@ "false"

-- | Convert primitive Boolean to Church Boolean
-- λb. test b tru fls
-- TODO: fake
churchbool :: UntypedLambda
churchbool = λ "b" . mkTest "b" tru fls

-- | equal returning a primitive boolean value
-- λm. λn. equal m n true false
realeq :: UntypedLambda
realeq = λ "m" . λ "n" . equal @@ "m" @@ "n" @@ "true" @@ "false"

-- | Convert a Church number to a primitive number
-- λm. m (λx. succ x) 0
realnat :: UntypedLambda
realnat = λ "m" . "m" @@ (λ "x" . "succ" @@ "x") @@ "0"

-- | a function that converts a primitive natural number into the corresponding Church number
--
-- match = iszro m
--
-- base  = λx
--
-- rec   = scc n (f (prd m))
--
-- TODO: fake
churchnat :: UntypedLambda
churchnat = mkFix "m" match base rec
  where
    match = iszro @@ "m"
    base = c 0
    rec = scc @@ "n" @@ ("f" @@ (prd @@ "m"))
