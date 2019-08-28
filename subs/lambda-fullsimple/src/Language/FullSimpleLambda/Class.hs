module Language.FullSimpleLambda.Class
  ( VarName,
    System (..)
    )
where

import RIO

type VarName = Text

class System t where

  data Term t :: *

  data Ty t :: *

  data Context t :: *

  data Pattern t :: *

  eval :: Term t -> Term t

  typeof :: Context t -> Term t -> Ty t

  desugar :: Term t -> Term t

  match :: Pattern t -> Term t -> (Term t -> Term t)
  match _ _ _ = error "Pattern matting is not implemented"
