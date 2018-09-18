{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Language.UntypedLambda.Lib.Base
  ( id
  , fix
  -- * helper
  , mkFix
  ) where

import RIO hiding (id, fix)

import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Types

-- | λx. x
id :: UntypedLambda
id = λ "x" "x"

-- | λf. (λx. f (λy. x x y)) (λx. f (λy. x x y))
fix :: UntypedLambda
fix = λ "f" $ t @@ t
  where
    t = λ "x" $ "f" @@ λ "y" ("x" @@ "x" @@ "y")

mkFix :: Text -> UntypedLambda -> UntypedLambda -> UntypedLambda -> UntypedLambda
mkFix v match base rec = fix @@ λ "f" (λ v $ mkTest match (λ "x" base) (λ "x" rec) @@ id)
