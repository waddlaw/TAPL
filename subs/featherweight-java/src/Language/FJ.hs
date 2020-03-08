module Language.FJ
  ( module Language.FJ.Example
  , module Language.FJ.Type
  , run
  , runAll
  , debug
  )
where

import Language.FJ.Example
import Language.FJ.Eval
import Language.FJ.Pretty
import Language.FJ.Type

import Prelude (putStrLn)
import RIO
import qualified RIO.Text as Text

run :: Program -> Text
run = pretty . uncurry eval

runAll :: Program -> [Text]
runAll = map pretty . uncurry evalTrace

debug :: Program -> IO ()
debug prog = do
  let steps = zip @Int [0..] (runAll prog)
  forM_ steps $ \(n, step) ->
    putStrLn $ mconcat [show n, ": ", Text.unpack step]
