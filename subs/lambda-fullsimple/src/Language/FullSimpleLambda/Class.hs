module Language.FullSimpleLambda.Class
  ( VarName,
    System (..),
  )
where

import Data.Kind
import RIO

type VarName = Text

class System t where
  data Term t :: Type
  data Ty t :: Type
  data Context t :: Type
  data Pattern t :: Type

  eval :: Term t -> Term t
  typeof :: Context t -> Term t -> Ty t
  desugar :: Term t -> Term t

  match :: Pattern t -> Term t -> (Term t -> Term t)
  match _ _ _ = error "Pattern matting is not implemented"
