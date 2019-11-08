module Language.Recon where

import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.List as List
import RIO.State

data Ty
  = TyArr Ty Ty
  | TyBool
  | TyNat
  | TyAtom TyVar
  deriving (Eq, Show, Ord)

newtype TyVar = TyVar Text
  deriving (Eq, Show, Ord)

data Term
  = TmVar Text
  | TmLam Text Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

-- 型代入
newtype TypeSubst = TypeSubst { unTS :: Map TyVar Ty }
  deriving (Eq, Show)

sigmaEx :: TypeSubst
sigmaEx = TypeSubst $
  Map.fromList [ (TyVar "X", TyBool)
               , (TyVar "Y", TyArr (TyAtom $ TyVar "X") (TyAtom $ TyVar "X"))
               , (TyVar "Z", TyBool)
               ]

-- dom sigmaEx == fromList [TyVar "X",TyVar "Y",TyVar "Z"]
dom :: TypeSubst -> Set TyVar
dom = Map.keysSet . unTS

-- range sigmaEx == fromList [TyArr (TyAtom (TyVar "X")) (TyAtom (TyVar "X")),TyBool]
range :: TypeSubst -> Set Ty
range = Set.fromList . Map.elems . unTS

class Apply a where
  apply :: TypeSubst -> a -> a

instance Apply Ty where
  apply :: TypeSubst -> Ty -> Ty
  apply ts@(TypeSubst s) = \case
    ty@(TyAtom tv) -> fromMaybe ty (Map.lookup tv s)
    TyNat -> TyNat
    TyBool -> TyBool
    TyArr ty1 ty2 -> TyArr (apply ts ty1) (apply ts ty2)

instance Apply Term where
  apply :: TypeSubst -> Term -> Term
  apply ts = \case
    TmVar i -> TmVar i
    TmLam x ty term -> TmLam x (apply ts ty) (apply ts term)
    TmApp t1 t2 -> TmApp (apply ts t1) (apply ts t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (apply ts t1) (apply ts t2) (apply ts t3)
    TmZero -> TmZero
    TmSucc t -> TmSucc (apply ts t)
    TmPred t -> TmPred (apply ts t)
    TmIsZero t -> TmIsZero (apply ts t)

-- example1 == TyArr TyBool TyBool
example1 :: Ty
example1 = apply sigma ty
  where
    -- σ = [X |-> Bool]
    sigma = TypeSubst (Map.singleton (TyVar "X") TyBool)
    -- X->X
    ty = TyArr (TyAtom $ TyVar "X") (TyAtom $ TyVar "X")

-- example2 == TyArr TyBool (TyArr (TyAtom (TyVar "X")) (TyAtom (TyVar "X")))
example2 :: Ty
example2 = apply sigma ty
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = TypeSubst (Map.fromList [ (TyVar "X", TyBool), (TyVar "Y", TyArr (TyAtom $ TyVar "X") (TyAtom $ TyVar "X"))])
    -- X->Y
    ty = TyArr (TyAtom $ TyVar "X") (TyAtom $ TyVar "Y")

-- exmaple3 == TmLam "x" TyBool (TmVar 0)
example3 :: Term
example3 = apply sigma term
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = TypeSubst (Map.fromList [(TyVar "X", TyBool)])
    -- λx:X. x
    term = TmLam "x" (TyAtom (TyVar "X")) (TmVar "x")

type ConstraintSet = [(Ty, Ty)]
type Context = [(Text, Ty)]
type ReturnType = Ty

runTypingC :: Term -> (ReturnType, Set TyVar , ConstraintSet)
runTypingC = flip evalState 1 . typingC [] []

ex22_3_3 :: Term
ex22_3_3 = TmLam "x" (tyv "X") . TmLam "y" (tyv "Y") . TmLam "z" (tyv "Z") $ body
  where
    tyv = TyAtom . TyVar
    body = TmApp t1 t2
    t1 = TmApp (TmVar "x") (TmVar "z")
    t2 = TmApp (TmVar "y") (TmVar "z")

typingC :: Context -> ConstraintSet -> Term -> State Int (ReturnType, Set TyVar , ConstraintSet)
typingC ctx cs = \case
  TmVar x -> do
    let ty = snd . fromMaybe (error "Variable is not found in context.") $ List.find ((==x) . fst) ctx
    return (ty, Set.empty, [])
  TmLam x ty t -> do
    (rt, tvs, c) <- typingC ((x,ty):ctx) cs t
    return (TyArr ty rt, tvs, c)
  TmApp t1 t2 -> do
    (rt1, tvs1, c1) <- typingC ctx cs t1
    (rt2, tvs2, c2) <- typingC ctx cs t2
    uniqueId <- get
    modify (+1)
    let
      tyvar = TyVar ("TYVAR" <> tshow uniqueId)
      rt = TyAtom tyvar
      tvs = tvs1 `Set.union` tvs2 `Set.union` Set.singleton tyvar
      c = c1 <> c2 <> [(rt1, TyArr rt2 rt)]
    return (rt, tvs, c)
  TmTrue -> return (TyBool, Set.empty, [])
  TmFalse -> return (TyBool, Set.empty, [])
  TmIf t1 t2 t3 -> do
    (rt1, tvs1, c1) <- typingC ctx cs t1
    (rt2, tvs2, c2) <- typingC ctx cs t2
    (rt3, tvs3, c3) <- typingC ctx cs t3
    let tvs = tvs1 `Set.union` tvs2 `Set.union` tvs3
        c = c1 <> c2 <> c3 <> [(rt1, TyBool), (rt2, rt3)]
    return (rt2, tvs, c)
  TmZero -> return (TyNat, Set.empty, [])
  TmSucc t -> do
    (rt, tvs, c) <- typingC ctx cs t
    return (TyNat, tvs, c <> [(rt, TyNat)])
  TmPred t -> do
    (rt, tvs, c) <- typingC ctx cs t
    return (TyNat, tvs, c <> [(rt, TyNat)])
  TmIsZero t -> do
    (rt, tvs, c) <- typingC ctx cs t
    return (TyBool, tvs, c <> [(rt, TyNat)])

{-
λ> runTypingC ex22_3_3 
(TyArr (TyAtom (TyVar "X")) (TyArr (TyAtom (TyVar "Y")) (TyArr (TyAtom (TyVar "Z")) (TyAtom (TyVar "TYVAR3"))))
,fromList [TyVar "TYVAR1",TyVar "TYVAR2",TyVar "TYVAR3"],
[(TyAtom (TyVar "X"),TyArr (TyAtom (TyVar "Z")) (TyAtom (TyVar "TYVAR1"))),(TyAtom (TyVar "Y"),TyArr (TyAtom (TyVar "Z")) (TyAtom (TyVar "TYVAR2"))),(TyAtom (TyVar "TYVAR1"),TyArr (TyAtom (TyVar "TYVAR2")) (TyAtom (TyVar "TYVAR3")))])

----
return type
(TyArr
  (TyAtom (TyVar "X"))
  (TyArr
    (TyAtom (TyVar "Y"))
    (TyArr
      (TyAtom (TyVar "Z"))
      (TyAtom (TyVar "TYVAR3")
    )
  )
),
X -> Y -> Z -> TYVAR3
----
型変数の集合
,fromList [TyVar "TYVAR1",TyVar "TYVAR2",TyVar "TYVAR3"],
----
制約集合
[ (TyAtom (TyVar "X"), TyArr (TyAtom (TyVar "Z")) (TyAtom (TyVar "TYVAR1")))
, (TyAtom (TyVar "Y"), TyArr (TyAtom (TyVar "Z")) (TyAtom (TyVar "TYVAR2")))
, (TyAtom (TyVar "TYVAR1"), TyArr (TyAtom (TyVar "TYVAR2")) (TyAtom (TyVar "TYVAR3")))
]

{ X = Z -> TYVAR1
, Y = Z -> TYVAR2
, TYVAR1 = TYVAR2 -> TYVAR3
}

-}