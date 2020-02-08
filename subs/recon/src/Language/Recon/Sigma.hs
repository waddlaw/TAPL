{-# LANGUAGE OverloadedStrings #-}
module Language.Recon.Sigma
  ( dom
  , range
  , example
  )
where

import RIO
import qualified RIO.Map  as Map
import qualified RIO.Set  as Set

import Language.Recon.Type

-- >>> dom example
-- fromList ["X","Y","Z"]
dom :: Sigma -> Set TyVarName
dom = Map.keysSet

-- >>> range example
-- fromList [TyArr (TyVar "X") (TyVar "X"),TyBool]
range :: Sigma -> Set Ty
range = Set.fromList . Map.elems

example :: Sigma
example = Map.fromList
  [ ("X", TyBool)
  , ("Y", TyArr (TyVar "X") (TyVar "X"))
  , ("Z", TyBool)
  ]