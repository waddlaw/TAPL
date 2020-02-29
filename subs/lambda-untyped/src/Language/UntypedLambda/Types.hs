{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda.Types
  ( Term (..)
  , UntypedLambda
  , (@@)
  , λ
  , Context
  , VarName
  , NamelessTerm (..)
  , getNlTermVar
  )
where

import RIO
import Data.Text.Prettyprint.Doc
import qualified RIO.Text as Text

type UntypedLambda = Term Text
type VarName = Text
-- | Contrary to books, ["x" , "y" , "z"] is indexed from left to right as [0, 1, 2]
type Context = [VarName]

data NamelessTerm
  = NlTmVar Int
  | NlTmLam NamelessTerm
  | NlTmApp NamelessTerm NamelessTerm
  deriving stock (Eq, Show)

getNlTermVar :: NamelessTerm -> Int
getNlTermVar (NlTmVar k) = k
getNlTermVar _ = error "panic"

instance IsString NamelessTerm where
  fromString = NlTmVar . fromMaybe 0 . readMaybe -- FIXME

infixl 9 @@

(@@) :: UntypedLambda -> UntypedLambda -> UntypedLambda
t1 @@ t2 = TmApp t1 t2

λ :: Text -> UntypedLambda -> UntypedLambda
λ = TmLam

data Term a
  = TmVar a
  | TmLam VarName (Term a)
  | TmApp (Term a) (Term a)
  deriving stock (Eq, Show)

instance Pretty UntypedLambda where
  pretty (TmVar x) = pretty x
  pretty (TmLam x t) = "λ" <> pretty x <> "." <+> pretty t
  pretty (TmApp t1 t2) = ppr t1 <+> ppr t2
    where
      ppr t@(TmVar _) = pretty t
      ppr t = parens (pretty t)

instance IsString UntypedLambda where
  fromString = TmVar . Text.pack
