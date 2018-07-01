{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.NB
  ( realbool
  , churchbool
  , realeq
  , realnat
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
