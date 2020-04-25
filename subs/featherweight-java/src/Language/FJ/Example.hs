{-# LANGUAGE OverloadedStrings #-}

module Language.FJ.Example
  ( exCT,
    exMain1,
    exMain2,
    exMain3,
    exMain4,
  )
where

import Language.FJ.Type
import RIO
import qualified RIO.Text as Text

{- The class table must satisfy all of the following conditions
(1) CT (C) = class C ... for C ∈ dom (CT)
(2) Object ∉ dom(CT)
(3) C ∈ dom (CT) for every class name C (except Object) appearing anywhere on CT
(4) There is no circulation in the subfunctional relationship created by CT, i.e. the <: relationship is antisymmetric.
-}
exCT :: ClassTable
exCT c = case getClassName c of
  "A" -> CL cA cObj [] (K cA [] [] []) []
  "B" -> CL cB cObj [] (K cB [] [] []) []
  "Pair" -> CL cP cObj [(cObj, fFst), (cObj, fSnd)] pairConstr [pairMethod]
  name -> error ("Can't find Class " ++ Text.unpack name ++ " in Class Tables.")
  where
    pairConstr = K cP [(cObj, fFst), (cObj, fSnd)] [] [(fFst, fFst), (fSnd, fSnd)]
    pairMethod = M cP (mkMethod "setfst") [(cObj, mkVar "newfst")] body
    body = TmNew cP [TmVar (mkVar "newfst"), TmFieldRef (TmVar (mkVar "this")) fSnd]
    cA = mkClass "A"
    cB = mkClass "B"
    cP = mkClass "Pair"
    cObj = mkClass "Object"
    fFst = mkField "fst"
    fSnd = mkField "snd"

-- | new Pair(new A(), new B()).setfst(new B())
exMain1 :: Term
exMain1 = TmMethodInv pAB (mkMethod "setfst") [b]

-- | ((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd
exMain2 :: Term
exMain2 = TmFieldRef cast (mkField "snd")
  where
    cast = TmCast (mkClass "Pair") m
    m = TmFieldRef p1 (mkField "fst")
    p1 = TmNew (mkClass "Pair") [pAB, a]

-- | new Pair(new A(), newB()).snd
exMain3 :: Term
exMain3 = TmFieldRef pAB (mkField "snd")

-- | (Pair)new Pair(new A(), new B())
exMain4 :: Term
exMain4 = TmCast (mkClass "Pair") pAB

-- utils
a , b, pAB :: Term
a = TmNew (mkClass "A") []
b = TmNew (mkClass "B") []
pAB = TmNew (mkClass "Pair") [a, b]
