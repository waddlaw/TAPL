-- {-# LANGUAGE ExistentialQuantification #-}
-- data T = forall a. MkT a

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
data T1 where
  T1 :: forall a.
    { a1 :: a
    , f1 :: a -> a
    } -> T1

p :: T1
p = T1 { a1=5, f1=(+1) }

data T2 where
  T2 :: forall a.
    { a2 :: a
    , f2 :: a -> Int
    } -> T2

p1 :: T2
p1 = T2 { a2=5, f2=(+1) }

data T3 where
  T3 :: forall a. a -> T3

p2 :: T3
p2 = T3 0

p3 :: T3
p3 = T3 True

p4 :: T2
p4 = T2 0 (+1)

p5 :: T2
p5 = T2 True (const 0)

p6 :: T1
p6 = T1 { a1=0, f1=(+1) }

data T4 where
  T4 :: forall a.
    { a4 :: a
    , f4 :: Int -> a
    } -> T4

p7 :: T4
p7 = T4 { a4=0, f4=(+1) }

data T5 where
  T5 :: { a5 :: Int, f5 :: Int -> Int } -> T5

p8 :: T5
p8 = T5 { a5=0, f5=(+1) }

-- λ> exP4_1 
-- 1
exP4_1 :: Int
exP4_1 =
  case p4 of
    T2 {..} -> f2 a2

-- λ> exP4_2
-- 1
exP4_2 :: Int
exP4_2 =
  case p4 of
    T2 a2 f2 -> (\y -> f2 y) a2

-- bad: a を具体的な数として扱うことはできない
-- exP4_3 :: Int
-- exP4_3 =
--   case p4 of
--     T2 {..} -> 1+a2

-- bad: 型がスコープ外
-- exP4_4 :: Int
-- exP4_4 =
--   case p4 of
--     T2 {..} -> a2

-- elimT2 :: (forall a. a -> (a -> Int) -> Int) -> T2 -> Int
-- elimT2 func (MkT2 a f) = func a f

data Counter where
  Counter :: forall a.
    { new :: a
    , get :: a -> Int
    , inc :: a -> a
    } -> Counter

counterADT :: Counter
counterADT = Counter
  { new = 1
  , get = id
  , inc = (+1)
  }

-- λ> counterEx1 
-- 2
counterEx1 :: Int
counterEx1 =
  case counterADT of
    Counter {..} -> get (inc new)

-- λ> counterEx2 
-- 4
counterEx2 :: Int
counterEx2 =
  case counterADT of
    Counter {..} ->
      let add3 = inc . inc . inc
       in get (add3 new)

data FlipFlop where
  FlipFlop :: forall a.
    { newFF  :: a
    , read   :: a -> Bool
    , toggle :: a -> a
    , reset  :: a -> a
    } -> FlipFlop

-- λ> counterEx3 
-- False
counterEx3 :: Bool
counterEx3 =
  case counterADT of
    Counter {..} ->
      let flipflop =
            FlipFlop
              { newFF  = new
              , read   = even . get
              , toggle = inc
              , reset  = const new
              }
       in case flipflop of
            FlipFlop {..} -> read (toggle (toggle newFF))

newtype RecordNat = RecordNat { x :: Int }
  deriving Show

counterADT2 :: Counter
counterADT2 = Counter
  { new = RecordNat { x=1 }
  , get = \RecordNat {..} -> x
  , inc = \RecordNat {..} -> RecordNat { x = x+1 }
  }

-- λ> exADT2 
-- 4
exADT2 :: Int
exADT2 =
  case counterADT2 of
    Counter {..} ->
      let add3 = inc . inc . inc
       in get (add3 new)

