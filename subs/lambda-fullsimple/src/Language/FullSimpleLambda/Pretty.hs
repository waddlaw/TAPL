module Language.FullSimpleLambda.Pretty
  ( prettyFullSimpleText
  , prettyType
  )
where

import Language.FullSimpleLambda.Types

import RIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

prettyFullSimpleText :: Context -> FullSimpleTypedLambda -> Text
prettyFullSimpleText ctx = renderStrict . layoutPretty defaultLayoutOptions . pprFullSimple ctx

prettyType :: Ty -> Text
prettyType = renderStrict . layoutPretty defaultLayoutOptions . pretty
