{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding ((.))
import Data.Function hiding ((.))
import Data.Kind

(.) :: a -> (a -> b) -> b
(.) = (&)

(∘) :: (t1 -> t2) -> (t3 -> t1) -> t3 -> t2
f ∘ g = \x -> f (g x)

class Class c a where
  data Methods c a :: Type
  m :: Methods c a
  m = error "Not exists method definition"

data Counter where
  Counter :: forall a. Class Counter a =>
    { state   :: a
    , methods :: Methods Counter a
    } -> Counter

instance Class Counter a where
  data Methods Counter a
    = M1 { get :: a -> Int
         , inc :: a -> a
         }

c :: Counter
c = Counter
  { state = 1 :: Int
  , methods = M1 { get = id, inc = (+1) }
  }

-- λ> c.sendGet
-- 1
sendGet :: Counter -> Int
sendGet = \case
  Counter {..} -> methods & \case
    M1 {..} -> get state

-- λ> c.sendInc.sendInc.sendGet
-- 3
sendInc :: Counter -> Counter
sendInc = \case
  Counter {..} -> methods & \case
    M1 {..} ->
      Counter
        { state = inc state
        , methods = methods
        }

data FlipFlop where
  FlipFlop :: forall a. Class FlipFlop a =>
    { stateFF  :: a
    , methodsFF :: Methods FlipFlop a
    } -> FlipFlop

instance Class FlipFlop a where
  data Methods FlipFlop a
    = M2 { read   :: a -> Bool
         , toggle :: a -> a
         , reset  :: a -> a
         }

flipflop :: FlipFlop
flipflop = FlipFlop
  { stateFF  = c
  , methodsFF = M2
                { read   = even ∘ sendGet
                , toggle = sendInc
                , reset  = const c
                }
  }

sendRead :: FlipFlop -> Bool
sendRead = \case
  FlipFlop {..} -> methodsFF & \case
    M2 {..} -> read stateFF

sendToggle :: FlipFlop -> FlipFlop
sendToggle = \case
  FlipFlop {..} -> methodsFF & \case
    M2 {..} -> FlipFlop
      { stateFF   = toggle stateFF
      , methodsFF = methodsFF
      }

sendReset :: FlipFlop -> FlipFlop
sendReset = \case
  FlipFlop {..} -> methodsFF & \case
    M2 {..} -> FlipFlop
      { stateFF   = reset stateFF
      , methodsFF = methodsFF
      }

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

data SubCounter
instance Class Counter a => Class SubCounter a where
  data Methods SubCounter a where
    SubCounter :: forall a. Class SubCounter a =>
      { state' :: a
      , methods' :: Methods SubCounter a
      } -> Methods SubCounter a

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