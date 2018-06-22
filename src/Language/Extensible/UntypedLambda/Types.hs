module Language.Extensible.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  ) where

import Language.UntypedLambda.Types (Strategy (..))

import           Data.Extensible
import Control.Lens    (( # ))

import           Data.String
import           Data.Text (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

newtype Term = Term
  { unwrapTerm :: Variant
      '[ "var"    >: Text
       , "lambda" >: (Text, Term)
       , "app"    >: (Term, Term)
       ]
   }

instance Pretty Term where
  pretty = matchField prettyTerm . unwrapTerm
    where
      prettyTerm = #var    @= pretty
                <: #lambda @= (\(x, t)   -> pretty "λ" <> pretty x <> pretty "." <+> pretty t)
                <: #app    @= (\(t1, t2) -> ppr t1 <+> ppr t2)
                <: nil

ppr :: Term -> Doc ann
ppr = matchField pm . unwrapTerm
  where
    pm = #var    @= pretty
      <: #lambda @= parens . pretty
      <: #app    @= parens . pretty
      <: nil

instance IsString Term where
  fromString v = Term $ #var # T.pack v