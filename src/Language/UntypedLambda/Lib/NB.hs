{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.NB
  ( realbool
  , churchbool
  , realeq
  , realnat
  -- * 演習5.2.10
  , churchnat
  ) where

import           Language.UntypedLambda.Prelude
import           Language.UntypedLambda.Types

-- | Church ブール値をプリミティブなブール値に変換
realbool :: UntypedLambda
realbool = TmLam "b" $ TmApp (TmApp "b" "true") "false"

-- | プリミティブなブール値を Church ブール値に変換
-- TODO: fake
churchbool :: UntypedLambda
churchbool = TmLam "b" $ TmApp (TmApp (TmApp test "b") tru) fls

-- | プリミティブなブール値を返す equal
realeq :: UntypedLambda
realeq = TmLam "m" $ TmLam "n" $ TmApp (TmApp (TmApp (TmApp equal "m") "n") "true") "false"

-- | Church 数からプリミティブな数への変換
realnat :: UntypedLambda
realnat = TmLam "m" $ TmApp (TmApp "m" (TmLam "x" (TmApp "succ" "x"))) "0"

-- | プリミティブな自然数を、対応する Church 数に変換する関数
-- | TODO: fake
churchnat :: UntypedLambda
churchnat = TmApp fix cn
  where
    cn = TmLam "f" $ TmLam "m" $ TmApp (TmApp (TmApp (TmApp test (TmApp iszro "m")) (TmLam "x" $ c 0)) t) (c 0)
    t  = TmLam "x" $ TmApp (TmApp scc "n") (TmApp "f" (TmApp prd "m"))