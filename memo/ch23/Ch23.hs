{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Ch23 where

import Prelude hiding (map, null, reverse)
import Data.Function

null :: [a] -> Bool
null [] = True
null _ = False

{-
map :: (x -> y) -> [x] -> [y]
map f = fix $ \m l ->
  if null l
    then []
    else (:) (f (head l))
             (m (tail l))
-}

map :: forall x y. (x -> y) -> [x] -> [y]
map (f :: x -> y) = fix $ \(m :: [x] -> [y]) (l :: [x]) ->
  if null @x l
    then [] @y
    else (:) @y (f (head @x l))
                (m (tail @x l))

{-
reverse :: [a] -> [a]
reverse = go []
  where
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs
-}

reverse :: forall a. [a] -> [a]
reverse =
  let go = fix $ \(m :: [x] -> [x] -> [x]) (acc :: [x]) (l :: [x]) ->
            if null @x l
              then acc
              else m ((:) @x (head @x l) acc)
                             (tail @x l)
  in go []

{- ex23.4.3 (O(n) バージョン)
reverse = let go =
  \X.
    (fix (\m: List X -> List X -> List X.
            \acc: List X.
              \l: List X.
                if isnil [X] l
                  then acc
                  else m (cons [X] (head [X] l) acc)
                         (tail [X] l)));
  in \X. \l:List X. go [X] (nil [X]) l

reverse: forall X. List X -> List X
-}

-- ex 23.4.4 挿入ソート
{-
isort :: (a -> a -> Bool) -> [a] -> [a]
isort _ [] = []
isort cmp (x:xs) = insert cmp x (isort cmp xs)
-}

isort :: forall a. (a -> a -> Bool) -> [a] -> [a]
isort cmp = fix $ \(m :: [a] -> [a]) (xs :: [a]) ->
  if null @a xs
    then [] @a
    else insert @a cmp (head @a xs)
                       (m (tail @a xs))

{-
isort = \X.
  \cmp: X -> X -> Bool.
    (fix (\m: List X -> List X.
            \l: List X.
              if isnil [X] l
                then nil [X]
                else insert [X] cmp (head [X] l)
                                    (m (tail [X] l))
         )
    )
-}

{-
insert :: (a -> a -> Bool) -> a -> [a] -> [a]
insert _ x [] = [x]
insert cmp x as@(y:ys) =
  if cmp x y
    then x : as
    else y : insert cmp x ys
-}

insert :: forall a. (a -> a -> Bool) -> a -> [a] -> [a]
insert cmp x = fix $ \(m :: [a] -> [a]) (ys :: [a]) ->
  if null @a ys
    then (:) @a x ([] @a)
    else if cmp x (head @a ys)
            then (:) @a x ys
            else (:) @a (head @a ys)
                        (m (tail @a ys))

{-
insert = \X.
  \cmp: X -> X -> Bool.
    \x: X.
      (fix (\m: List X -> List X.
              \l: List X.
                if isnil [X] l
                  then cons [X] x (nil [X])
                  else if cmp x (head [X] l)
                         then cons [X] x l
                         else cons [X] (head [X] l)
                                       (m (tail [X] l))
           )
      );
-}

{- ex23.4.5
and = \p:CBool. \q:CBool. \X. \t:X. \f:X. p [X] (q [X] t f) f;
and : CBool -> CBool
-}

{- ex23.4.6
iszro = \c: CNat. c [CBool] (\b:CBool. fls) tru
iszro : CNat -> CBool
-}

{- ex23.4.7
ctimes c2 c4
  = \X. \s:X->X. c4 [X] (c2 [X] s)
  = \X. \s:X->X. (\X. \s:X->X. \z:X. s (s (s (s z)))) [X] (c2 [X] s)
  = \X. \s:X->X. (\s:X->X. \z:X. s (s (s (s z)))) (c2 [X] s)
  = \X. \s:X->X. (\s:X->X. \z:X. s (s (s (s z)))) (c2 [X] s)
  = \X. \s:X->X. (\s:X->X. \z:X. s (s (s (s z)))) ((\X. \s:X->X. \z:X. s (s z)) [X] s)
  = \X. \s:X->X. (\s:X->X. \z:X. s (s (s (s z)))) ((\s:X->X. \z:X. s (s z)) s)
  = \X. \s:X->X. (\s:X->X. \z:X. s (s (s (s z)))) (\z:X. s (s z))
  = \X. \s:X->X.
      (\z:X. (\z:X. s (s z)) (
        (\z:X. s (s z)) (
          (\z:X. s (s z)) (
            (\z:X. s (s z)) z)
          )
        )
      ))
  = \X. \s:X->X.
      (\z:X. (\z:X. s (s z)) (
        (\z:X. s (s z)) (
          (\z:X. s (s z)) (
            s (s z)
          )
        )
      ))
  = \X. \s:X->X.
      (\z:X. (\z:X. s (s z)) (
        (\z:X. s (s z)) (
          s (s (s (s z)))
        )
      ))
  = \X. \s:X->X.
      (\z:X. (\z:X. s (s z)) (
        s (s (s (s (s (s z)))))
      ))
  = \X. \s:X->X. (\z:X. s (s (s (s (s (s (s (s z))))))))
  = c8

cexp c2 c4
  = \X. c4 [X->X] (c2 [X]))
  = \X. (\X. \s:X->X. \z:X. s (s (s (s z)))) [X->X] ((\X. \s:X->X. \z:X. s (s z)) [X])
  = \X. (\s:(X->X)->X->X. \z:X->X. s (s (s (s z)))) (\s:X->X. \z:X. s (s z))
  = \X. (\s':X->X. s (s (s ((\s:X->X. \z:X. s (s z)) s'))))
  = \X. (\s':X->X. s (s (s (\z:X. s' (s' z)))))
  = \X. (\s':X->X. s (s ((\s:X->X. \z:X. s (s z)) (\z:X. s' (s' z)))))
  = \X. (\s':X->X. s (s (\z:X. (\z:X. s' (s' z)) ((\z:X. s' (s' z)) z)))))
  = \X. (\s':X->X. s (s (\z:X. (\z:X. s' (s' z)) (s' (s' z)))))
  = \X. (\s':X->X. s (s (\z:X. (s' (s' (s' (s' z)))))))
  = \X. (\s':X->X. s ((\s:X->X. \z:X. s (s z)) (\z:X. (s' (s' (s' (s' z)))))))
  = \X. (\s':X->X. s ((\z:X. (\z:X. (s' (s' (s' (s' z))))) ((\z:X. (s' (s' (s' (s' z))))) z))))
  = \X. (\s':X->X. s ((\z:X. (\z:X. (s' (s' (s' (s' z))))) (s' (s' (s' (s' z)))))))
  = \X. (\s':X->X. s ((\z:X. (s' (s' (s' (s' (s' (s' (s' (s' z)))))))))))
  = ...
-}

{- ex23.4.8
pairNat = \m:CNat. \n:CNat. \X. \f:CNat->CNat->X. f m n
pairNat : CNat -> CNat -> PairNat

fstNat = \p:PairNat. p [CNat] (tru [CNat])
fstNat : PariNat -> CNat

sndNat = \p:PairNat. p [CNat] (fls [CNat])
sndNat : PairNat -> CNat
-}

{- ex23.4.9
f = \p:PairNat. \X. \f:CNat->CNat->X. p (csucc (fstNat p)) (fstNat p)
f : PairNat -> PairNat

prd = \n:CNat. \X. \s:X->X. \z:X. sndNat (n [X] f (pairNat c0 c0))
prd : CNat -> CNat
-}

{- ex23.4.10
vpred = \n:CNat . \X. \s:X->X .
  \z:X .
    n [(X->X)->X]
      (\p:(X->X)->X . \q:X->X . q (p s))
      (\x:X->X. z)
      (\x:X. x);
vpred : CNat -> CNat

vpred c1
  = \X. \s:X->X.
      \z:X. c1 [(X->X)->X]
              (\p:(X->X)->X. \q:X->X. q (p s))
              (\x:X->X. z)
              (\x:X. x);
  = \X. \s:X->X.
      \z:X . (\s':(X->X)->X)->(X->X)->X. \z:X. s' z)
              (\p:(X->X)->X. \q:X->X. q (p s))
              (\x:X->X. z)
              (\x:X. x);
  = \X. \s:X->X.
      \z:X. (\z:X. (\p:(X->X)->X. \q:X->X. q (p s)) z)
              (\x:X->X. z)
              (\x:X. x);
  = \X. \s:X->X.
      \z:X. (\p:(X->X)->X. \q:X->X. q (p s))
              (\x:X->X. z)
              (\x:X. x);
  = \X. \s:X->X.
      \z:X. (\q:X->X. q ((\x:X->X. z) s))
              (\x:X. x);
  = \X. \s:X->X.
      \z:X. (\q:X->X. q (z)) -- ここで s が捨てられる！！！
              (\x:X. x);
  = \X. \s:X->X.
      \z:X. ((\x:X. x) (z))
  = \X. \s:X->X. \z:X. z
  = c0
-}