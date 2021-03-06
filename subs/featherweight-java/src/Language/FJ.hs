module Language.FJ
  ( module Language.FJ.Example,
    module Language.FJ.Eval,
    module Language.FJ.Parser,
    module Language.FJ.Type,
    run,
    runAll,
    debug,
  )
where

import Language.FJ.Eval
import Language.FJ.Example
import Language.FJ.Parser
import Language.FJ.Pretty
import Language.FJ.Type
import RIO
import qualified RIO.Text as Text
import Prelude (putStrLn)

run :: Program -> Text
run = renderFJ . uncurry eval

runAll :: Program -> [Text]
runAll = map renderFJ . uncurry evalTrace

debug :: Program -> IO ()
debug prog = do
  let steps = zip @Int [0 ..] (runAll prog)
  forM_ steps $ \(n, step) ->
    putStrLn $ mconcat [show n, ": ", Text.unpack step]
