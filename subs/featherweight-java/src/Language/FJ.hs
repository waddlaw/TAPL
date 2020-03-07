{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.FJ
  ( run
  , runAll
  , example
  , example2
  )
where

import Language.FJ.Type

import RIO hiding (to)
import qualified RIO.List.Partial as List'
import qualified RIO.Text as Text

import Prelude (putStrLn)

isValue :: Term -> Bool
isValue = \case
  TmNew _ ts -> all isValue ts
  _          -> False

-- =======================================
-- = Figure 19-2: auxiliary definitions
-- =======================================

{- |
>>> fields exCT (mkClass "Pair")
[(mkClass "Object",FN "fst"),(mkClass "Object",FN "snd")]
-}
fields :: ClassTable -> Class -> [(Class, Field)]
fields ct = \case
  (getClassName -> "Object") -> []
  c -> let CL _ d cfs _ _ = ct c
        in cfs ++ fields ct d

{- |
>>> mtype exCT (MN "setfst") (CN "Pair")
([CN "Object"],CN "Pair")
-}
-- TODO
-- mtype :: CT -> Method -> Class -> Maybe ([Class], Class)
-- mtype ct m = fmap f . mhelper ct m
--   where
--     f (M rt _ args _) = (map fst args, rt)

{- |
>>> mbody exCT (MN "setfst") (CN "Pair")
([VN "newfst"],TmNew (CN "Pair") [TmVar (VN "newfst"),TmFieldRef (TmVar (VN "this")) (FN "snd")])
-}
mbody :: ClassTable -> Method -> Class -> Maybe ([Var], Term)
mbody ct m = fmap f . mhelper ct m
  where
    f (M _ _ args term) = (map snd args, term)

mhelper :: ClassTable -> Method -> Class -> Maybe MethodDef
mhelper ct m = \case
  (getClassName -> "Object") -> Nothing
  c -> let CL _ d _cfs _ ms = ct c
        in maybe (mhelper ct m d) Just $ findMethodDef m ms

eqMethodDef :: Method -> MethodDef -> Bool
eqMethodDef m1 (M _ m2 _ _) = m1 == m2

findMethodDef :: Method -> [MethodDef] -> Maybe MethodDef
findMethodDef m ms
    | null md   = Nothing
    | otherwise = Just (List'.head md)
  where
    md = filter (eqMethodDef m) ms

-- TODO
-- override :: CT -> Method -> Class -> ([Class], Class) -> Bool
-- override ct m d (cs, c0) = case mtype ct m d of
--   Nothing       -> True
--   Just (ds, d0) -> cs == ds && c0 == d0

-- ============================
-- = Figure 19-3: evaluation
-- ============================

{- |
>>> uncurry eval example 
TmNew (mkClass "Pair")
  [ TmNew (mkClass "B") []
  , TmFieldRef (TmNew (mkClass "Pair") [TmNew (mkClass "A") []
  , TmNew (mkClass "B") []]) (FN "snd")
  ]
-}
eval :: ClassTable -> Term -> Term
eval ct t
  | isValue t = t
  | otherwise = eval ct (eval' ct t)

evalTrace :: ClassTable -> Term -> IO ()
evalTrace ct t = do
  putStrLn . Text.unpack $ pretty t
  if isValue t
    then return ()
    else evalTrace ct (eval' ct t)

eval' :: ClassTable -> Term -> Term
eval' ct = \case
  TmVar _ -> error "TODO: TmVar"
  -- E-NEW-ARG
  TmNew c ts ->
    -- ti:rest is absolutely successful
    let (vs, ti:rest) = span isValue ts
     in TmNew c (vs ++ (eval ct ti:rest))

  TmFieldRef t fi ->
    if isValue t
      -- E-PROJNEW
      then
        let TmNew c vs = t
         in fst . List'.head . dropWhile ((/=fi) . snd . snd) $ zip vs $ fields ct c
      -- E-FIELD
      else TmFieldRef (eval ct t) fi

  TmMethodInv t m ts ->
    if isValue t
      then
        if all isValue ts
          -- E-INVKNEW
          then
            let this@(TmNew c _vs) = t
                (xs, t0) = fromMaybe (error $ "E-INVKNEW: " <> show m <> ", " <> show c) $ mbody ct m c
             in subst ((mkVar "this", this):zip xs ts) t0
          -- E-INVK-ARG
          else 
            -- ti:rest is absolutely successful
            let (vs, ti:rest) = span isValue ts
             in TmMethodInv t m (vs ++ (eval ct ti:rest))
      -- E-INVK-RECV
      else TmMethodInv (eval ct t) m ts

  TmCast d@c t ->
    if isValue t
      -- E-CASTNEW
      then let TmNew c' _vs = t
            in if checkCast ct c' d then t else error "E-CASTNEW"
      -- E-CAST
      else TmCast c (eval ct t)

subst :: [(Var, Term)] -> Term -> Term
subst fs = \case
  TmVar x            -> fromMaybe (error "subst") $ lookup x fs
  TmFieldRef t field -> TmFieldRef (subst fs t) field
  TmMethodInv t m ts -> TmMethodInv (subst fs t) m (map (subst fs) ts)
  TmNew  c ts        -> TmNew c (map (subst fs) ts)
  TmCast c t         -> TmCast c (subst fs t)

checkCast :: ClassTable -> Class -> Class -> Bool
checkCast ct from to = from == to || checkCast ct d to
  where
    CL _ d _ _ _ = ct from

-- ============================
-- = Sample programs
-- ============================

example :: Program
example = (exCT, mainMethod)
  where
    mainMethod = TmMethodInv p (mkMethod "setfst") [b]
    p = TmNew (mkClass "Pair") [a, b]
    a = TmNew (mkClass "A") []
    b = TmNew (mkClass "B") []

example2 :: Program
example2 = (exCT, mainMethod)
  where
    mainMethod = TmFieldRef cast (mkField "snd")
    cast = TmCast (mkClass "Pair") m
    m    = TmFieldRef p1 (mkField "fst")
    p1   = TmNew (mkClass "Pair") [p2, a]
    p2   = TmNew (mkClass "Pair") [a, b]
    a    = TmNew (mkClass "A") []
    b    = TmNew (mkClass "B") [] 

{- The class table must satisfy all of the following conditions
(1) CT (C) = class C ... for C ∈ dom (CT)
(2) Object ∉ dom(CT)
(3) C ∈ dom (CT) for every class name C (except Object) appearing anywhere on CT
(4) There is no circulation in the subfunctional relationship created by CT, i.e. the <: relationship is antisymmetric.
-}
exCT :: ClassTable
exCT c = case getClassName c of
  "A"    -> CL (mkClass "A") (mkClass "Object") [] (K (mkClass "A") []) []
  "B"    -> CL (mkClass "B") (mkClass "Object") [] (K (mkClass "B") []) []
  "Pair" -> CL (mkClass "Pair") (mkClass "Object")
               [ (mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd") ]
               pairConstr
               [pairMethod]
  name   -> error ("Can't find Class " ++ Text.unpack name ++ " in Class Tables.")
  where
    pairConstr = K (mkClass "Pair") [ (mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd") ]
    pairMethod = M (mkClass "Pair") (mkMethod "setfst") [(mkClass "Object", mkVar "newfst")] body
    body       = TmNew (mkClass "Pair") [TmVar (mkVar "newfst"), TmFieldRef (TmVar (mkVar "this")) (mkField "snd")]

pretty :: Term -> Text
pretty = \case
  TmVar var -> getVarName var
  TmFieldRef t field -> pretty t <> "." <> getFieldName field
  TmMethodInv t method args
    -> pretty t <> "." <> getMethodName method <> "(" <> Text.intercalate ", " (map pretty args) <> ")"
  TmNew  cls args -> "new " <> getClassName cls <> "(" <> Text.intercalate ", " (map pretty args) <> ")"
  TmCast cls t -> "(" <> getClassName cls <> ")" <> pretty t

run :: Program -> Text
run = pretty . uncurry eval

runAll :: Program -> IO ()
runAll = uncurry evalTrace

{-
*Language.FJ Data.Maybe Data.List> runAll example
new Pair(new A(), new B()).setfst(new B())
new Pair(new B(), new Pair(new A(), new B()).snd)
new Pair(new B(), new B())

*Language.FJ Data.Maybe Data.List> runAll example2
(Pair)new Pair(new Pair(new A(), new B()), new A()).fst.snd
new Pair(new A(), new B()).snd
new B()
-}