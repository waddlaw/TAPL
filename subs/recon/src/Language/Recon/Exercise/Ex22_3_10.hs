{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Recon.Exercise.Ex22_3_10
  ( runTypingC
  , ex22_3_3
  )
where

import RIO
import qualified RIO.List as List
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyVar VarName  -- ^ TyId
  deriving (Eq, Show, Ord)

data Term
  = TmVar VarName
  | TmLam VarName Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

type ConstraintSet = Set (Ty, Ty)
type Context = [(VarName, Ty)]
type VarName = Text
type ReturnType = Ty
type TyVarIdStream = [VarName]

-- >>> runTypingC (TmApp TmZero TmTrue)
-- ( TyVar "?X_1" 
-- , fromList 
--    [ 
--        ( TyNat
--        , TyArr TyBool ( TyVar "?X_1" )
--        ) 
--    ]
--)
runTypingC :: Term -> (ReturnType, ConstraintSet)
runTypingC = extract . typingC [] tyVarIdStream
  where
    tyVarIdStream = map (("?X_" <>) . tshow @Int) [1..]
    -- stream を捨てないと、結果を表示する際に計算が止まらなくなるので注意
    extract (ty, _, constr) = (ty, constr)

-- 演習 22.3.9 のアルゴリズム
typingC ::
  Context ->
  TyVarIdStream -> -- フレッシュな変数を作るための引数が増えた
  Term ->
  (ReturnType, TyVarIdStream, ConstraintSet) -- 前回は State モナドを使っていた
typingC ctx varIds = \case
  TmVar x ->
    let msg = "Variable '" <> Text.unpack x <> "' is not found in context: " <> show ctx
        ty = fromMaybe (error msg) $ List.lookup x ctx
     in (ty, varIds, Set.empty)
  TmLam x ty t ->
    let (rt, restVarIds, c) = typingC ((x,ty):ctx) varIds t
     in (TyArr ty rt, restVarIds, c)
  TmApp t1 t2 ->
    let (rt1, restVarIds1, c1) = typingC ctx varIds t1
        (rt2, (x:restVarIds2), c2) = typingC ctx restVarIds1 t2
        rt = TyVar x
        c = c1 <> c2 <> Set.singleton (rt1, TyArr rt2 rt)
     in (rt, restVarIds2, c)
  TmTrue  -> (TyBool, varIds, Set.empty)
  TmFalse -> (TyBool, varIds, Set.empty)
  TmIf t1 t2 t3 ->
    let (rt1, restVarIds1, c1) = typingC ctx varIds t1
        (rt2, restVarIds2, c2) = typingC ctx restVarIds1 t2
        (rt3, restVarIds3, c3) = typingC ctx restVarIds2 t3
        c = c1 <> c2 <> c3 <> Set.fromList [(rt1, TyBool), (rt2, rt3)]
     in (rt2, restVarIds3, c)
  TmZero -> (TyNat, varIds, Set.empty)
  TmSucc t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
     in (TyNat, restVarIds, c <> Set.singleton (rt, TyNat))
  TmPred t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
     in (TyNat, restVarIds, c <> Set.singleton (rt, TyNat))
  TmIsZero t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
     in (TyBool, restVarIds, c <> Set.singleton (rt, TyNat))

-- examples
ex22_3_3 :: Term
ex22_3_3 = TmLam "x" (TyVar "X") . TmLam "y" (TyVar "Y") . TmLam "z" (TyVar "Z") $ body
  where
    body = TmApp t1 t2
    t1 = TmApp (TmVar "x") (TmVar "z")
    t2 = TmApp (TmVar "y") (TmVar "z")

{-
λ> runTypingC ex22_3_3 
( TyArr ( TyVar "X" ) 
    ( TyArr ( TyVar "Y" ) 
        ( TyArr ( TyVar "Z" ) ( TyVar "?X_3" ) )
    )
, fromList 
    [ 
        ( TyVar "?X_1" 
        , TyArr ( TyVar "?X_2" ) ( TyVar "?X_3" )
        ) 
    , 
        ( TyVar "X" 
        , TyArr ( TyVar "Z" ) ( TyVar "?X_1" )
        ) 
    , 
        ( TyVar "Y" 
        , TyArr ( TyVar "Z" ) ( TyVar "?X_2" )
        ) 
    ] 
)
-}