{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Function hiding ((.))
import Data.Kind
import Prelude hiding ((.))

(.) :: a -> (a -> b) -> b
(.) = (&)

(∘) :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
f ∘ g = \x -> f (g x)

class Class c a where
  data Methods c a :: Type

data ClassDef c a = ClassDef
  { state :: a,
    methods :: Methods c a
  }

data Counter

instance Class Counter a where
  data Methods Counter a where
    CounterM ::
      forall a.
      Class Counter a =>
      { get :: a -> Int,
        inc :: a -> a
      } ->
      Methods Counter a

c :: ClassDef Counter Int
c =
  ClassDef
    { state = 1,
      methods = CounterM {get = id, inc = (+ 1)}
    }

-- λ> c.sendGet
-- 1
sendGet :: ClassDef Counter a -> Int
sendGet = \case
  ClassDef {..} ->
    methods & \case
      CounterM {..} -> get state

-- λ> c.sendInc.sendInc.sendGet
-- 3
sendInc :: ClassDef Counter a -> ClassDef Counter a
sendInc = \case
  ClassDef {..} ->
    methods & \case
      CounterM {..} ->
        ClassDef
          { state = inc state,
            methods = methods
          }

data SubCounter

instance Class Counter a => Class SubCounter a where
  data Methods SubCounter a where
    SubCounterM ::
      forall a.
      Class Counter a =>
      { dec :: a -> a
      } ->
      Methods SubCounter a

subc :: ClassDef SubCounter Int
subc =
  ClassDef
    { state = 1,
      methods = SubCounterM {dec = subtract 1}
    }

-- instance Class Counter a => Class SubCounter a where
--   data Methods SubCounter a where
--     SubCounter :: forall a. Class SubCounter a =>
--       { state' :: a
--       , methods' :: Methods SubCounter a
--       } -> Methods SubCounter a

-- instance Class Counter a where
--   data Methods Counter a
--     = M1 { get :: a -> Int
--          , inc :: a -> a
--          }

-- data FlipFlop where
--   FlipFlop :: forall a. Class FlipFlop a =>
--     { stateFF  :: a
--     , methodsFF :: Methods FlipFlop a
--     } -> FlipFlop

-- instance Class FlipFlop a where
--   data Methods FlipFlop a
--     = M2 { read   :: a -> Bool
--          , toggle :: a -> a
--          , reset  :: a -> a
--          }

-- flipflop :: FlipFlop
-- flipflop = FlipFlop
--   { stateFF  = c
--   , methodsFF = M2
--                 { read   = even ∘ sendGet
--                 , toggle = sendInc
--                 , reset  = const c
--                 }
--   }

-- sendRead :: FlipFlop -> Bool
-- sendRead = \case
--   FlipFlop {..} -> methodsFF & \case
--     M2 {..} -> read stateFF

-- sendToggle :: FlipFlop -> FlipFlop
-- sendToggle = \case
--   FlipFlop {..} -> methodsFF & \case
--     M2 {..} -> FlipFlop
--       { stateFF   = toggle stateFF
--       , methodsFF = methodsFF
--       }

-- sendReset :: FlipFlop -> FlipFlop
-- sendReset = \case
--   FlipFlop {..} -> methodsFF & \case
--     M2 {..} -> FlipFlop
--       { stateFF   = reset stateFF
--       , methodsFF = methodsFF
--       }

{-
λ> flipflop.sendToggle.sendToggle.sendRead
False
λ> flipflop.sendToggle.sendReset.sendRead
False
λ> flipflop.sendToggle.sendToggle.sendReset.sendToggle.sendRead
True
-}

-- data SubCounter where
--   SubCounter :: forall a. Class SubCounter a =>
--     { state' :: a
--     , methods' :: Methods SubCounter a
--     } -> SubCounter

-- data SubCounter
-- instance Class Counter a => Class SubCounter a where
--   data Methods SubCounter a where
--     SubCounter :: forall a. Class SubCounter a =>
--       { state' :: a
--       , methods' :: Methods SubCounter a
--       } -> Methods SubCounter a

-- sendDec :: SubCounter -> SubCounter
-- sendDec = \case
--   SubCounter {..} -> methods' & \case
--     M3 {..} ->
--       SubCounter
--         { state' = dec state'
--         , methods' = methods'
--         }

-- sendGet' :: Counter -> Int
-- sendGet' = \case
--   Counter {..} -> methods & \case
--     M1 {..} -> get state
