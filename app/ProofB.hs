module Main (main) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Language.B
import qualified Language.B.Example as B
import Language.Core
import RIO
import qualified RIO.List as List
import RIO.Orphans ()
import qualified RIO.Partial as RIO'
import RIO.State
import System.Console.Haskeline
import Prelude (print, putStrLn)

type Proof = InputT (StateT EvalRelation IO) ()

main :: IO ()
main = do
  putStrLn "Start ProofB"
  _ <- runStateT (runInputT defaultSettings main') B.example
  putStrLn "Leaving ProofB"

main' :: Proof
main' = do
  minput <- getInputLine "proofB> "
  case trim <$> minput of
    Nothing -> return ()
    Just ":q" -> return ()
    Just ":rules" -> rulesCmd >> main'
    Just ":set" -> setTargetCmd >> main'
    Just ":show" -> showTargetCmd >> main'
    Just input ->
      if  | ":step" `List.isPrefixOf` input -> stepCmd input >> main'
          | otherwise -> helpCmd >> main'

helpCmd :: Proof
helpCmd = mapM_ outputStrLn $ "available commands" : commands

commands :: [String]
commands =
  [ "  :set",
    "  :show",
    "  :step",
    "  :rules",
    "  :q"
  ]

setTargetCmd :: Proof
setTargetCmd = getInputLine "[term -> term]: " >>= \case
  Nothing -> return ()
  Just input ->
    case bparser input of
      Left err -> outputStrLn err
      Right er -> do
        putPretty er
        lift $ put er

showTargetCmd :: Proof
showTargetCmd = lift get >>= putPretty

stepCmd :: String -> Proof
stepCmd input =
  case stepCmdParser input of
    Left err -> outputStrLn err
    Right n ->
      if n `notElem` map fromEnum allRules
        then outputStrLn ("invalid input range: " ++ show n) >> rulesCmd
        else do
          let rule = RIO'.toEnum n
          er <- lift get
          case deduce rule er of
            Nothing -> outputStrLn "Proof Complete!"
            Just derivedER -> do
              lift $ put derivedER
              putPretty er
              putPretty @String "======================= E-IF"
              putPretty derivedER

rulesCmd :: Proof
rulesCmd = mapM_ (outputStrLn . renderRule) allRules

allRules :: [Rule]
allRules = [minBound .. maxBound]

-------
renderRule :: Rule -> String
renderRule r = mconcat [n, ": ", render r]
  where
    n = show . fromEnum $ r

putPretty :: Pretty a => a -> Proof
putPretty = lift . lift . print . renderString . layoutPretty defaultLayoutOptions . pretty
