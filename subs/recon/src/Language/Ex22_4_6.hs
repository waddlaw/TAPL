module Ex22_4_6 where

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyId String
  deriving (Eq, Show, Ord)

-- | 制約集合
type Constr = [(Ty, Ty)]

unify :: Constr -> Constr
unify [] = []
unify ((s, t):c')
  | s == t  = unify c'
  | isVar s && s `notInFv` t =
      let sigma = (s, t)
      in  sigma : unify (map (applyC sigma) c')
  | isVar t && t `notInFv` s =
      let sigma = (t, s)
      in  sigma : unify (map (applyC sigma) c')
  | isArr s && isArr t =
      let TyArr s1 s2 = s
          TyArr t1 t2 = t
      in  unify ([(s1, t1), (s2, t2)] ++ c')
  | otherwise = error "fail"

-- utils
isVar :: Ty -> Bool
isVar (TyId _) = True
isVar _ = False

isArr :: Ty -> Bool
isArr (TyArr _ _) = True
isArr _ = False

notInFv :: Ty -> Ty -> Bool
notInFv (TyId x) t = x `notElem` (fv t)
notInFv _ _ = False

fv :: Ty -> [String]
fv (TyId x) = [x]
fv (TyArr ty1 ty2) = fv ty1 ++ fv ty2
fv _ = []

getVar :: Ty -> String
getVar (TyId x) = x

applyC :: (Ty, Ty) -> (Ty, Ty) -> (Ty, Ty)
applyC sigma (s, t) = (apply sigma s, apply sigma t)

apply :: (Ty, Ty) -> Ty -> Ty
apply _ TyBool = TyBool
apply _ TyNat  = TyNat
apply sigma (TyArr ty1 ty2) = TyArr (apply sigma ty1) (apply sigma ty2)
apply (s, t) u
  | s == u    = t
  | otherwise = u

-- examples
{-
λ> unify ex22_4_3_1
[(TyId "X",TyNat),(TyId "Y",TyArr TyNat TyNat)]
-}
ex22_4_3_1 :: Constr
ex22_4_3_1 = [(x, TyNat), (TyId "Y", TyArr x x)]
  where
    x = TyId "X"

{-
λ> unify ex22_4_3_2
[(TyId "X",TyNat),(TyId "Y",TyNat)]
-}
ex22_4_3_2 :: Constr
ex22_4_3_2 = [ (TyArr TyNat TyNat, TyArr (TyId "X") (TyId "Y")) ]

{-
λ> unify ex22_4_3_3
[ (TyId "Z",TyArr (TyId "U") (TyId "W"))
, (TyId "X",TyId "Y")
, (TyId "Y",TyArr (TyId "U") (TyId "W"))
]
-}
ex22_4_3_3 :: Constr
ex22_4_3_3 = [ (TyArr (TyId "X") (TyId "Y"), TyArr (TyId "Y") (TyId "Z"))
              , (TyId "Z", TyArr (TyId "U") (TyId "W"))
              ]

{-
λ> unify ex22_4_3_4
*** Exception: fail
-}
ex22_4_3_4 :: Constr
ex22_4_3_4 = [ (TyNat, TyArr TyNat (TyId "Y")) ]

{-
λ> unify ex22_4_3_5
*** Exception: fail
-}
ex22_4_3_5 :: Constr
ex22_4_3_5 = [ (TyId "Y", TyArr TyNat (TyId "Y")) ]

{-
λ> unify ex22_4_3_6
[]
-}
ex22_4_3_6 :: Constr
ex22_4_3_6 = []

ex :: Constr
ex = [ (TyNat, TyNat) ]

{-
λ> unify ex22_5_2 
[ (TyId "X"    , TyArr (TyId "Z")    (TyId "?X_1"))
, (TyId "Y"    , TyArr (TyId "Z")    (TyId "?X_2"))
, (TyId "?X_1" , TyArr (TyId "?X_2") (TyId "?X_3"))
]
-}
ex22_5_2 :: Constr
ex22_5_2 = [ (TyId "X"    , TyArr (TyId "Z")    (TyId "?X_1"))
           , (TyId "Y"    , TyArr (TyId "Z")    (TyId "?X_2"))
           , (TyId "?X_1" , TyArr (TyId "?X_2") (TyId "?X_3"))
           ]