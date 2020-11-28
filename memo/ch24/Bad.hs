{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

data T where
  T :: forall a. a -> (a -> Int) -> T

t1 :: T
t1 = T 0 (+ 1)

good :: Int
good =
  case t1 of
    T x y -> 0

bad :: Int
bad =
  let T x y = t1
   in 0

-- bad :: Int
-- bad = 0
--   where
--     T x y = t1
