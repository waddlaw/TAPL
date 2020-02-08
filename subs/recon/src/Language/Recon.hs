{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Recon where

import RIO
import qualified RIO.List as List
import qualified RIO.Set  as Set
import qualified RIO.Text as Text
import RIO.State

import Language.Recon.Type

runTypingC :: Term -> (ReturnType, Set TyVarName, ConstraintSet)
runTypingC = flip evalState 1 . typingC []

typingC ::
  Context ->
  Term ->
  State Int (ReturnType, Set TyVarName, ConstraintSet)
typingC ctx = \case
  TmVar x -> do
    let msg = "Variable '" <> Text.unpack x <> "' is not found in context: " <> show ctx
        ty = fromMaybe (error msg) $ List.lookup x ctx
    return (ty, Set.empty, Set.empty)
  TmLam x ty t -> do
    (rt, tvs, c) <- typingC ((x,ty):ctx) t
    return (TyArr ty rt, tvs, c)
  TmApp t1 t2 -> do
    (rt1, tvs1, c1) <- typingC ctx t1
    (rt2, tvs2, c2) <- typingC ctx t2
    x <- getUniqueId
    let
      rt = TyVar x
      tvs = tvs1 <> tvs2 <> Set.singleton x
      c = c1 <> c2 <> Set.singleton (rt1, TyArr rt2 rt)
    return (rt, tvs, c)
  TmTrue  -> return (TyBool, Set.empty, Set.empty)
  TmFalse -> return (TyBool, Set.empty, Set.empty)
  TmIf t1 t2 t3 -> do
    (rt1, tvs1, c1) <- typingC ctx t1
    (rt2, tvs2, c2) <- typingC ctx t2
    (rt3, tvs3, c3) <- typingC ctx t3
    let tvs = tvs1 <> tvs2 <> tvs3
        c = c1 <> c2 <> c3 <> Set.fromList [(rt1, TyBool), (rt2, rt3)]
    return (rt2, tvs, c)
  TmZero -> return (TyNat, Set.empty, Set.empty)
  TmSucc t -> do
    (rt, tvs, c) <- typingC ctx t
    return (TyNat, tvs, c <> Set.singleton (rt, TyNat))
  TmPred t -> do
    (rt, tvs, c) <- typingC ctx t
    return (TyNat, tvs, c <> Set.singleton (rt, TyNat))
  TmIsZero t -> do
    (rt, tvs, c) <- typingC ctx t
    return (TyBool, tvs, c <> Set.singleton (rt, TyNat))
  TmFix t -> do
    (rt1, tvs, c) <-typingC ctx t
    x <- getUniqueId
    let rt = TyVar x
    return (rt, tvs, c <> Set.singleton (rt1, TyArr rt rt))

-- utils

getUniqueId :: State Int Text
getUniqueId = do
  uniqueId <- get
  modify (+1)
  return ("?X_" <> tshow uniqueId)

-- examples

{-
>>> runTypingC ex22_3_3
( TyArr ( TyVar "X" ) 
    ( TyArr ( TyVar "Y" ) 
        ( TyArr ( TyVar "Z" ) ( TyVar "TYVAR3" ) )
    )
, fromList 
    [ "TYVAR1" 
    , "TYVAR2" 
    , "TYVAR3" 
    ] 
, fromList 
    [ 
        ( TyVar "TYVAR1" 
        , TyArr ( TyVar "TYVAR2" ) ( TyVar "TYVAR3" )
        ) 
    , 
        ( TyVar "X" 
        , TyArr ( TyVar "Z" ) ( TyVar "TYVAR1" )
        ) 
    , 
        ( TyVar "Y" 
        , TyArr ( TyVar "Z" ) ( TyVar "TYVAR2" )
        ) 
    ] 
)
-}
ex22_3_3 :: Term
ex22_3_3 = TmLam "x" (TyVar "X") . TmLam "y" (TyVar "Y") . TmLam "z" (TyVar "Z") $ body
  where
    body = TmApp t1 t2
    t1 = TmApp (TmVar "x") (TmVar "z")
    t2 = TmApp (TmVar "y") (TmVar "z")

{-
>>> runTypingC example1
( TyVar "?X_1" 
, fromList [ "?X_1" ]
, fromList 
    [ 
        ( TyNat
        , TyArr TyBool ( TyVar "?X_1" )
        ) 
    ]
)
-}
example1 :: Term
example1 = TmApp TmZero TmTrue

{-
>>> runTypingC example2
( TyVar "?X_1" 
, fromList [ "?X_1" ]
, fromList 
    [ 
        ( TyArr TyBool TyBool
        , TyArr TyNat ( TyVar "?X_1" )
        ) 
    ]
)
-}
example2 :: Term
example2 = TmApp (TmLam "x" TyBool (TmVar "x")) TmZero

{-
>>> runTypingC example3
( TyArr 
    ( TyArr ( TyVar "X" ) ( TyVar "Y" ) ) ( TyVar "?X_1" )
, fromList [ "?X_1" ]
, fromList 
    [ 
        ( TyArr ( TyVar "X" ) ( TyVar "Y" )
        , TyArr TyNat ( TyVar "?X_1" )
        ) 
    ]
) 
-}
example3 :: Term
example3 = TmLam "x" (TyArr (TyVar "X") (TyVar "Y")) $ body
  where
    body = TmApp (TmVar "x") TmZero