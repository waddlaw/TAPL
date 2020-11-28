{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Function
import Prelude hiding (read)

data Counter where
  Counter ::
    forall a.
    { state :: a,
      methods :: CounterMethods a
    } ->
    Counter

data CounterMethods a = CounterMethods
  { get :: a -> Int,
    inc :: a -> a
  }

sendGet :: Counter -> Int
sendGet = \case
  Counter {..} ->
    methods & \case
      CounterMethods {..} -> get state

sendInc :: Counter -> Counter
sendInc = \case
  Counter {..} ->
    methods & \case
      CounterMethods {..} ->
        Counter
          { state = inc state,
            methods = methods
          }

c :: Counter
c =
  Counter
    { state = 1,
      methods =
        CounterMethods
          { get = id,
            inc = (+ 1)
          }
    }

data FlipFlop where
  FlipFlop ::
    forall a.
    { stateFF :: a,
      methods' :: FFMethods a
    } ->
    FlipFlop

data FFMethods a = FFMethods
  { read :: a -> Bool,
    toggle :: a -> a,
    reset :: a -> a
  }

sendRead :: FlipFlop -> Bool
sendRead = \case
  FlipFlop {..} ->
    methods' & \case
      FFMethods {..} -> read stateFF

sendToggle :: FlipFlop -> FlipFlop
sendToggle = \case
  FlipFlop {..} ->
    methods' & \case
      FFMethods {..} ->
        FlipFlop
          { stateFF = toggle stateFF,
            methods' = methods'
          }

flipflop :: FlipFlop
flipflop =
  FlipFlop
    { stateFF = c,
      methods' =
        FFMethods
          { read = even . sendGet,
            toggle = sendInc,
            reset = const c
          }
    }

-- λ> ffEx (flipflop c)
-- False
ffEx :: FlipFlop -> Bool
ffEx = sendRead . sendToggle . sendToggle
