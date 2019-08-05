module Language.SimpleLambda.Pretty
  ( prettySimpleText
    )
where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.SimpleLambda.Types
import RIO

prettySimpleText :: Context -> SimpleTypedLambda -> Text
prettySimpleText ctx = renderStrict . layoutCompact . pprSimple ctx
