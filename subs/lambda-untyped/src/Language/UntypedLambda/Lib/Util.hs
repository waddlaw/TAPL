module Language.UntypedLambda.Lib.Util
  ( (.),
  )
where

import Prelude hiding ((.))

infixr 5 .

(.) :: (a -> b) -> a -> b
(.) = ($)
