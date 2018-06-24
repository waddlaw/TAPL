module Extensible.Language.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  , lambda
  , var
  , app
  ) where

import           Language.UntypedLambda.Types (Strategy (..))

import           Data.Extensible
import           Control.Lens    (( # ))

import           Data.String
import           Data.Text (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

lambda :: Text -> Term -> Term
lambda v t = Term $ #lambda # (v, t)

var :: Text -> Term
var v = Term $ #var # v

app :: Term -> Term -> Term
app t1 t2 = Term $ #app # (t1, t2)

newtype Term = Term
  { unwrapTerm :: Variant
      '[ "var"    >: Text
       , "lambda" >: (Text, Term)
       , "app"    >: (Term, Term)
       ]
   } deriving (Eq, Show)

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
  fromString = var . T.pack