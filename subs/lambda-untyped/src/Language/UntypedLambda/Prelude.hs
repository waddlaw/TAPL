{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda.Prelude
  ( prelude
    )
where

import Language.Core
import Language.UntypedLambda.Lib.Base
import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Church
import Language.UntypedLambda.Lib.Either
import Language.UntypedLambda.Lib.List
import Language.UntypedLambda.Lib.Pair
import Language.UntypedLambda.Types
import qualified RIO.Map as Map

prelude :: Prelude UntypedLambda
prelude =
  Map.fromList
    [ ("id", id),
      ("tru", tru),
      ("fls", fls),
      ("test", test),
      ("and", and),
      ("or", or),
      ("not", not),
      ("pair", pair),
      ("fst", fst),
      ("snd", snd),
      ("scc", scc),
      ("plus", plus),
      ("times", times),
      ("power", power1),
      ("iszro", iszro),
      ("prd", prd),
      ("subtract", subtract1),
      ("equal", equal),
      ("left", left),
      ("right", right),
      ("isLeft", isLeft),
      ("nil", nil),
      ("cons", cons),
      ("isnil", isnil),
      ("head", head),
      ("tail", tail)
      ]
