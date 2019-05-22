{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda.Lib.Either
  ( left
  , right
  , isLeft
  ) where

import Prelude hiding ((.))

import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Util
import Language.UntypedLambda.Types

{-
data Either a b
  = Left a
  | Right b
-}

-- Left :: a -> Either a b
left :: UntypedLambda
left = λ "a". λ "l". λ "r". "l" @@ "a"

-- Right :: b -> Either a b
right :: UntypedLambda
right = λ "b". λ "l". λ "r". "r" @@ "b"

{-
isLeft :: Either a b -> Bool
isLeft = \e -> case e of
  Left  -> \a -> True
  Right -> \b -> False 
-}
isLeft :: UntypedLambda
isLeft = λ "e". "e" @@ (λ "a". tru) @@ (λ "b". fls)