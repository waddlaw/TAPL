{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.UntypedLambda.Lib.NB
  ( realbool
  , churchbool
  , realeq
  , realnat
  -- * 演習5.2.10
  , churchnat
  ) where

import RIO

import           Language.UntypedLambda.Lib.Base
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Types

-- | Church ブール値をプリミティブなブール値に変換
-- λb. b true false
realbool :: UntypedLambda
realbool = λ "b" $ "b" @@ "true" @@ "false"

-- | プリミティブなブール値を Church ブール値に変換
-- λb. test b tru fls
-- TODO: fake
churchbool :: UntypedLambda
churchbool = λ "b" $ mkTest "b" tru fls

-- | プリミティブなブール値を返す equal
-- λm. λn. equal m n true false
realeq :: UntypedLambda
realeq = λ "m" $ λ "n" $ equal @@ "m" @@ "n" @@ "true" @@ "false"

-- | Church 数からプリミティブな数への変換
-- λm. m (λx. succ x) 0
realnat :: UntypedLambda
realnat = λ "m" $ "m" @@ λ "x" ("succ" @@ "x") @@ "0"

-- | プリミティブな自然数を、対応する Church 数に変換する関数
-- match = iszro m
-- base  = λx
-- rec   = scc n (f (prd m))
-- | TODO: fake
churchnat :: UntypedLambda
churchnat = mkFix "m" match base rec
  where
    match = iszro @@ "m"
    base = c 0
    rec  = scc @@ "n" @@ ("f" @@ (prd @@ "m"))
