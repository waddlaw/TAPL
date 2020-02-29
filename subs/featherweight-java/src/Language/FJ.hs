{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.FJ
  ( run
  , runAll
  , example
  , example2
  )
where

import RIO hiding (to)
import qualified RIO.List as List
import qualified RIO.List.Partial as List.Partial
import Prelude (putStrLn)

type Program = (CT, Term)

-- | Class Table
type CT = Class -> ClassDef

-- | Class Declaration
data ClassDef = CL
  Class             -- ^ Self class name
  Class             -- ^ Super class name
  [(Class, Field)]  -- ^ Field declarations
  ConstDef          -- ^ Constructor declarations
  [MethodDef]       -- ^ Method declarations
  deriving stock (Eq, Show)

-- | Constructor Declarations
data ConstDef = K
  Class            -- ^ Constructor name
  [(Class, Field)] -- ^ The fields used to initialize the fields of the instance. The first part contains the fields for the superclass
  deriving stock (Eq, Show)

-- | Method Declarations
data MethodDef = M
  Class          -- ^ The name of the return type (Class)
  Method         -- ^ method name
  [(Class, Var)] -- ^ Method arguments (Argument type and argument variable name)
  Term           -- ^ Method body
  deriving stock (Eq, Show)

data Term
  = TmVar Var                      -- ^ Variable
  | TmFieldRef Term Field          -- ^ field access
  | TmMethodInv Term Method [Term] -- ^ method invocation
  | TmNew Class [Term]             -- ^ object creation
  | TmCast Class Term              -- ^ cast
  deriving (Eq, Show)

-- | It can be just a String, but it seems to be wrong, so I chose newtype.
newtype Class  = CN String deriving stock (Eq, Show)
newtype Method = MN String deriving stock (Eq, Show)
newtype Field  = FN String deriving stock (Eq, Show)
newtype Var    = VN String deriving stock (Eq, Show)

isValue :: Term -> Bool
isValue = \case
  TmNew _ ts -> all isValue ts
  _          -> False

-- =======================================
-- = Figure 19-2: auxiliary definitions
-- =======================================

{- |
>>> fields exCT (CN "Pair")
[(CN "Object",FN "fst"),(CN "Object",FN "snd")]
-}
fields :: CT -> Class -> [(Class, Field)]
fields ct = \case
  CN "Object" -> []
  c           -> let CL _ d cfs _ _ = ct c
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
mbody :: CT -> Method -> Class -> Maybe ([Var], Term)
mbody ct m = fmap f . mhelper ct m
  where
    f (M _ _ args term) = (map snd args, term)

mhelper :: CT -> Method -> Class -> Maybe MethodDef
mhelper ct m = \case
  CN "Object" -> Nothing
  c           -> let CL _ d _cfs _ ms = ct c
                  in maybe (mhelper ct m d) pure $ findMethodDef m ms

eqMethodDef :: Method -> MethodDef -> Bool
eqMethodDef m1 (M _ m2 _ _) = m1 == m2

findMethodDef :: Method -> [MethodDef] -> Maybe MethodDef
findMethodDef m ms
    | null md   = Nothing
    | otherwise = Just (List.Partial.head md)
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
TmNew (CN "Pair") [TmNew (CN "B") [],TmFieldRef (TmNew (CN "Pair") [TmNew (CN "A") [],TmNew (CN "B") []]) (FN "snd")]
-}
eval :: CT -> Term -> Term
eval ct t
  | isValue t = t
  | otherwise = eval ct $ eval' ct t

evalTrace :: CT -> Term -> IO ()
evalTrace ct t = do
  putStrLn $ pretty t
  if isValue t
    then return ()
    else evalTrace ct (eval' ct t)

eval' :: CT -> Term -> Term
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
         in fst . List.Partial.head . dropWhile ((/=fi) . snd . snd) $ zip vs $ fields ct c
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
             in subst ((VN "this", this):zip xs ts) t0
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
  TmNew c ts         -> TmNew c (map (subst fs) ts)
  TmCast c t         -> TmCast c (subst fs t)

checkCast :: CT -> Class -> Class -> Bool
checkCast ct from to = from == to || checkCast ct d to
  where
    CL _ d _ _ _ = ct from

-- ============================
-- = Sample programs
-- ============================

example :: Program
example = (exCT, mainMethod)
  where
    mainMethod = TmMethodInv p (MN "setfst") [b]
    p = TmNew (CN "Pair") [a, b]
    a = TmNew (CN "A") []
    b = TmNew (CN "B") []

example2 :: Program
example2 = (exCT, mainMethod)
  where
    mainMethod = TmFieldRef cast (FN "snd")
    cast = TmCast (CN "Pair") m
    m    = TmFieldRef p1 (FN "fst")
    p1   = TmNew (CN "Pair") [p2, a]
    p2   = TmNew (CN "Pair") [a, b]
    a    = TmNew (CN "A") []
    b    = TmNew (CN "B") [] 

{- The class table must satisfy all of the following conditions
(1) CT (C) = class C ... for C ∈ dom (CT)
(2) Object ∉ dom(CT)
(3) C ∈ dom (CT) for every class name C (except Object) appearing anywhere on CT
(4) There is no circulation in the subfunctional relationship created by CT, i.e. the <: relationship is antisymmetric.
-}
exCT :: CT
exCT (CN name) = case name of
  "A"    -> CL (CN "A") (CN "Object") [] (K (CN "A") []) []
  "B"    -> CL (CN "B") (CN "Object") [] (K (CN "B") []) []
  "Pair" -> CL (CN "Pair") (CN "Object")
               [ (CN "Object", FN "fst") , (CN "Object", FN "snd") ]
               pairConstr
               [pairMethod]
  _ -> error ("Can't find Class " ++ name ++ " in Class Tables.")
  where
    pairConstr = K (CN "Pair") [ (CN "Object", FN "fst"), (CN "Object", FN "snd") ]
    pairMethod = M (CN "Pair") (MN "setfst") [(CN "Object", VN "newfst")] body
    body       = TmNew (CN "Pair") [TmVar (VN "newfst"), TmFieldRef (TmVar (VN "this")) (FN "snd")]

pretty :: Term -> String
pretty = \case
  TmVar (VN x)              -> x
  TmFieldRef t (FN f)       -> pretty t <> "." <> f
  TmMethodInv t (MN m) args -> pretty t <> "." <> m <> "(" <> List.intercalate ", " (map pretty args) <> ")"
  TmNew (CN c) args         -> "new " <> c <> "(" <> List.intercalate ", " (map pretty args) <> ")"
  TmCast (CN c) t           -> "(" <> c <> ")" <> pretty t

run :: Program -> String
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