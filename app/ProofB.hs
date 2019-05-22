module Main (main) where

import Prelude

import Language.Core

import Language.B
import qualified Language.B.Example as B

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import System.Console.Haskeline

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
    Nothing       -> return ()
    Just ":q"     -> return ()
    Just ":rules" -> rulesCmd      >> main'
    Just ":set"   -> setTargetCmd  >> main'
    Just ":show"  -> showTargetCmd >> main'

    Just input ->
      if | ":step" `isPrefixOf` input -> stepCmd input >> main'
         | otherwise -> helpCmd >> main'

helpCmd :: Proof
helpCmd = mapM_ outputStrLn $ "available commands" : commands

commands :: [String]
commands =
  [ "  :set"
  , "  :show"
  , "  :step"
  , "  :rules"
  , "  :q"
  ]

setTargetCmd :: Proof
setTargetCmd = do
  minput <- getInputLine "[term -> term]: "
  case minput of
    Nothing -> return ()
    Just input ->
      case bparser input of
        Left err -> outputStrLn err
        Right er -> do
          putPretty er
          lift $ put er

showTargetCmd :: Proof
showTargetCmd = do
  el <- lift get
  putPretty el

stepCmd :: String -> Proof
stepCmd input =
  case stepCmdParser input of
    Left err -> outputStrLn err
    Right n ->
      if n `notElem` map fromEnum allRules then
        do
          outputStrLn $ "invalid input range: " ++ show n
          rulesCmd
      else
        do
          let rule = toEnum n
          er <- lift get
          case deduce rule er of
            Nothing -> outputStrLn "Proof Complete!"
            Just derivedER -> do
              lift $ put derivedER

              putPretty er
              putPretty ("======================= E-IF" :: String)
              putPretty derivedER

rulesCmd :: Proof
rulesCmd = mapM_ (outputStrLn . renderRule) allRules

allRules :: [Rule]
allRules = [minBound..maxBound]

-------
renderRule :: Rule -> String
renderRule r = mconcat [n, ": ", render r]
  where n = show . fromEnum $ r

putPretty :: Pretty a => a -> Proof
putPretty = lift . lift . print . renderString . layoutPretty defaultLayoutOptions . pretty
