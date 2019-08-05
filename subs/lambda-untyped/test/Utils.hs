module Utils
  ( evalAllStrategy
    )
where

import Language.Core.Types
import Language.UntypedLambda
import Test.Tasty.HUnit

evalAllStrategy :: UntypedLambda -> UntypedLambda -> IO ()
evalAllStrategy term expected = do
  eval NormalOrder term @?= expected
  eval CallByName term @?= expected
  eval CallByValue term @?= expected
