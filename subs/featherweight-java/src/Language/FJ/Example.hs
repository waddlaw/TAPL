{-# LANGUAGE OverloadedStrings #-}
module Language.FJ.Example
  ( exCT
  , exMain1
  , exMain2
  , exMain3
  , exMain4
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
  "A"    -> CL (mkClass "A") (mkClass "Object") [] (K (mkClass "A") [] [] []) []
  "B"    -> CL (mkClass "B") (mkClass "Object") [] (K (mkClass "B") [] [] []) []
  "Pair" -> CL (mkClass "Pair") (mkClass "Object")
               [ (mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd") ]
               pairConstr
               [pairMethod]
  name   -> error ("Can't find Class " ++ Text.unpack name ++ " in Class Tables.")
  where
    pairConstr = K (mkClass "Pair")
                   [ (mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd") ]
                   []
                   [ (mkField "fst", mkField "fst"), (mkField "snd", mkField "snd") ]
    pairMethod = M (mkClass "Pair") (mkMethod "setfst") [(mkClass "Object", mkVar "newfst")] body
    body       = TmNew (mkClass "Pair") [TmVar (mkVar "newfst"), TmFieldRef (TmVar (mkVar "this")) (mkField "snd")]

-- | new Pair(new A(), new B()).setfst(new B())
exMain1 :: Term
exMain1 = TmMethodInv p (mkMethod "setfst") [b]
  where
    p = TmNew (mkClass "Pair") [a, b]
    a = TmNew (mkClass "A") []
    b = TmNew (mkClass "B") []

-- | ((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd
exMain2 :: Term
exMain2 = TmFieldRef cast (mkField "snd")
  where
    cast = TmCast (mkClass "Pair") m
    m    = TmFieldRef p1 (mkField "fst")
    p1   = TmNew (mkClass "Pair") [p2, a]
    p2   = TmNew (mkClass "Pair") [a, b]
    a    = TmNew (mkClass "A") []
    b    = TmNew (mkClass "B") [] 

-- | new Pair(new A(), newB()).snd
exMain3 :: Term
exMain3 = TmFieldRef p (mkField "snd")
  where
    p = TmNew (mkClass "Pair") [a, b]
    a = TmNew (mkClass "A") []
    b = TmNew (mkClass "B") []

-- | (Pair)new Pair(new A(), new B())
exMain4 :: Term
exMain4 = TmCast (mkClass "Pair") p
  where
    p = TmNew (mkClass "Pair") [a, b]
    a = TmNew (mkClass "A") []
    b = TmNew (mkClass "B") []
