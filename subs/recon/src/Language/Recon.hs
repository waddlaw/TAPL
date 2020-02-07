{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Recon where

import RIO
import qualified RIO.List as List
import qualified RIO.Map  as Map
import qualified RIO.Set  as Set
import RIO.State

data Ty
  = TyArr Ty Ty
  | TyBool
  | TyNat
  | TyVar Text
  deriving (Eq, Show, Ord)

data Term
  = TmVar Text
  | TmLam Var Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

type Context = [(Var, Ty)]
type Var = Text
type TyVar = Text

-- 型代入
type Sigma = Map TyVar Ty

sigmaEx :: Sigma
sigmaEx = Map.fromList
  [ ("X", TyBool)
  , ("Y", TyArr (TyVar "X") (TyVar "X"))
  , ("Z", TyBool)
  ]

-- dom sigmaEx == fromList ["X","Y","Z"]
dom :: Sigma -> Set TyVar
dom = Map.keysSet

-- range sigmaEx == fromList [TyArr (TyVar "X") (TyVar "X"),TyBool]
range :: Sigma -> Set Ty
range = Set.fromList . Map.elems

class TySubst a where
  tySubst :: Sigma -> a -> a

instance TySubst Ty where
  tySubst :: Sigma -> Ty -> Ty
  tySubst sigma = \case
    ty@(TyVar x) -> fromMaybe ty (Map.lookup x sigma)
    TyNat -> TyNat
    TyBool -> TyBool
    TyArr ty1 ty2 -> TyArr (tySubst sigma ty1) (tySubst sigma ty2)

instance TySubst Term where
  tySubst :: Sigma -> Term -> Term
  tySubst sigma = \case
    TmVar i -> TmVar i
    TmLam x ty t -> TmLam x (tySubst sigma ty) (tySubst sigma t)
    TmApp t1 t2 -> TmApp (tySubst sigma t1) (tySubst sigma t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (tySubst sigma t1) (tySubst sigma t2) (tySubst sigma t3)
    TmZero -> TmZero
    TmSucc t -> TmSucc (tySubst sigma t)
    TmPred t -> TmPred (tySubst sigma t)
    TmIsZero t -> TmIsZero (tySubst sigma t)

instance TySubst Context where
  tySubst :: Sigma -> Context -> Context
  tySubst sigma = map (second (tySubst sigma))

-- example0 == [("x",TyBool),("y",TyBool),("x",TyVar "Y")]
example0 :: Context
example0 = tySubst sigma ctx
  where
    -- σ = [X |-> Bool]
    sigma = Map.singleton "X" TyBool
    -- X->X
    ctx = [("x", TyVar "X"), ("y", TyVar "X"), ("x", TyVar "Y")]

-- example1 == TyArr TyBool TyBool
example1 :: Ty
example1 = tySubst sigma ty
  where
    -- σ = [X |-> Bool]
    sigma = Map.singleton "X" TyBool
    -- X->X
    ty = TyArr (TyVar "X") (TyVar "X")

-- example2 == TyArr TyBool (TyArr (TyVar "X") (TyVar "X"))
example2 :: Ty
example2 = tySubst sigma ty
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = Map.fromList [ ("X", TyBool), ("Y", TyArr (TyVar "X") (TyVar "X"))]
    -- X->Y
    ty = TyArr (TyVar "X") (TyVar "Y")

-- example3 == TmLam "x" TyBool (TmVar "x")
example3 :: Term
example3 = tySubst sigma term
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = Map.fromList [("X", TyBool)]
    -- λx:X. x
    term = TmLam "x" (TyVar "X") (TmVar "x")

type ConstraintSet = Set (Ty, Ty)
type ReturnType = Ty

runTypingC :: Term -> (ReturnType, Set TyVar, ConstraintSet)
runTypingC = flip evalState 1 . typingC []

ex22_3_3 :: Term
ex22_3_3 = TmLam "x" (TyVar "X") . TmLam "y" (TyVar "Y") . TmLam "z" (TyVar "Z") $ body
  where
    body = TmApp t1 t2
    t1 = TmApp (TmVar "x") (TmVar "z")
    t2 = TmApp (TmVar "y") (TmVar "z")

typingC ::
  Context ->
  Term ->
  State Int (ReturnType, Set TyVar, ConstraintSet)
typingC ctx = \case
  TmVar x -> do
    let ty = fromMaybe (error "Variable is not found in context.") $ List.lookup x ctx
    return (ty, Set.empty, Set.empty)
  TmLam x ty t -> do
    (rt, tvs, c) <- typingC ((x,ty):ctx) t
    return (TyArr ty rt, tvs, c)
  TmApp t1 t2 -> do
    (rt1, tvs1, c1) <- typingC ctx t1
    (rt2, tvs2, c2) <- typingC ctx t2
    uniqueId <- get
    modify (+1)
    let
      tyvar = "TYVAR" <> tshow uniqueId
      rt = TyVar tyvar
      tvs = tvs1 <> tvs2 <> Set.singleton tyvar
      c = c1 <> c2 <> Set.singleton (rt1, TyArr rt2 rt)
    return (rt, tvs, c)
  TmTrue -> return (TyBool, Set.empty, Set.empty)
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

{-
λ> runTypingC ex22_3_3
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
)

結果の型: X -> Y -> Z -> TYVAR3
型変数の集合: {TYVAR1, TYVAR2, TYVAR3}
制約集合: { X = Z -> TYVAR1
         , Y = Z -> TYVAR2
         , TYVAR1 = TYVAR2 -> TYVAR3
         }
}
-}

{-
λ> runTypingC (TmApp TmZero TmTrue)
( TyVar "TYVAR1" 
, fromList [ "TYVAR1" ]
, fromList 
    [ 
        ( TyNat
        , TyArr TyBool ( TyVar "TYVAR1" )
        ) 
    ]
)

結果の型: TYVAR1
型変数の集合: { TYVAR1 }
制約集合: { Nat = Bool -> TYVAR1 }
-}

{-
λ> runTypingC (TmApp (TmLam "x" TyBool (TmVar "x")) TmZero)
( TyVar "TYVAR1" 
, fromList [ "TYVAR1" ]
, fromList 
    [ 
        ( TyArr TyBool TyBool
        , TyArr TyNat ( TyVar "TYVAR1" )
        ) 
    ]
)
)

結果の型: TYVAR1
型変数の集合: { TYVAR1 }
制約集合: { Bool -> Bool = Nat -> TYVAR1 }
-}

example4 :: Term
example4 = TmLam "x" (TyArr (TyVar "X") (TyVar "Y")) $ body
  where
    body = TmApp (TmVar "x") TmZero

{-
λ> runTypingC example4
( TyArr 
    ( TyArr ( TyVar "X" ) ( TyVar "Y" ) ) ( TyVar "TYVAR1" )
, fromList [ "TYVAR1" ]
, fromList 
    [ 
        ( TyArr ( TyVar "X" ) ( TyVar "Y" )
        , TyArr TyNat ( TyVar "TYVAR1" )
        ) 
    ]
)

結果の型: (X -> Y) -> TYVAR1
型変数の集合: { TYVAR1 }
制約集合: { X -> Y = Nat -> TYVAR1 }
-}