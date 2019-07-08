{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda.Lib.Pair
  ( pair
  , fst
  , snd
  , -- * helper
    mkPair
  )
where

import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Util
import Language.UntypedLambda.Types
import Prelude hiding ((.))

-- | λf. λs. λb. b f s
pair :: UntypedLambda
pair = λ "f" . λ "s" . λ "b" . "b" @@ "f" @@ "s"

-- | λf. λs. pair f s
mkPair :: UntypedLambda -> UntypedLambda -> UntypedLambda
mkPair f s = pair @@ f @@ s

-- | λp. p tru
fst :: UntypedLambda
fst = λ "p" . "p" @@ tru

-- | λp. p fls
snd :: UntypedLambda
snd = λ "p" . "p" @@ fls
