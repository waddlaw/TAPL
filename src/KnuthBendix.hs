module KnuthBendix where

import Data.Bifunctor

nth :: [a] -> Int -> a
nth = (!!)

forall :: (a -> Bool) -> [a] -> Bool
forall = all

multiForall :: (a -> b -> Bool) -> [a] -> [b] -> Bool
multiForall p xs = and . zipWith p xs

exists :: (a -> Bool) -> [a] -> Bool
exists = any

-- TODO
allapp = undefined

data Vname = Vname String Int
  deriving (Eq, Ord)

data Term
  = V Vname
  | T String [Term]
  deriving Eq

-- TODO
printTerm = undefined

type Subst = [(Vname, Term)]

getVname :: (Vname, Term) -> Vname
getVname = fst

getTerm :: (Vname, Term) -> Term
getTerm = snd

emptySubst :: Subst
emptySubst = []

addSubst :: Subst -> Vname -> Term -> Subst
addSubst s x t = (x, t) : s

indom :: Vname -> Subst -> Bool
indom x = exists ((x==) . fst)

app :: Subst -> Vname -> Term
app ((y,t):rest) x
  | x == y = t
  | otherwise = app rest x

lift :: Subst -> Term -> Term
lift s (V x) = if indom x s then app s x else V x
lift s (T f ts) = T f (map (lift s) ts)

occurs :: Vname -> Term -> Bool
occurs x (V y) = x == y
occurs x (T _ ts) = exists (occurs x) ts

type InternalSubst = Subst

solve :: [(Term, Term)] -> InternalSubst -> InternalSubst
solve [] s = s
solve ((V x, t):rest) s = if V x == t then solve rest s else elim x t rest s
solve ((t, V x):rest) s = elim x t rest s
solve ((T f ts, T g us):rest) s = if f == g then solve (zip ts us ++ rest) s else error "UNIFY"

elim :: Vname -> Term -> [(Term, Term)] -> InternalSubst -> InternalSubst
elim x t rest s
  | occurs x t = error "UNIFY"
  | otherwise  = solve (map (bimap xt xt) rest) ((x,t): map (second xt) s)
  where
    xt = lift $ addSubst emptySubst x t

unify :: (Term, Term) -> Subst
unify (t1, t2) = solve [(t1, t2)] []

matchs :: [(Term, Term)] -> Subst -> Subst
matchs [] s = s
matchs ((V x, t):rest) s =
  if indom x s
  then if app s x == t
       then matchs rest s
       else error "UNIFY"
  else matchs rest (addSubst s x t)
matchs ((t, V x):rest) s = error "UNIFY"
matchs ((T f ts, T g us):rest) s =
  if f == g
  then matchs (zip ts us ++ rest) s
  else error "UNIFY"