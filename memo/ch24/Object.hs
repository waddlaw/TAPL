{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

import Data.Function

data Counter where
  Counter :: forall a.
    { state :: a
    , methods :: Methods a
    } -> Counter

data Methods a = Methods
  { get :: a -> Int
  , inc :: a -> a
  }

c :: Counter
c = Counter
  { state = 5
  , methods = Methods
                { get = id
                , inc = (+1)
                }
  }

-- λ> exC
-- 5
exC :: Int
exC = c & \case
  Counter {..} -> methods & \case
    Methods {..} -> get state

-- λ> sendGet c
-- 5
sendGet :: Counter -> Int
sendGet = \case
  Counter {..} -> methods & \case
    Methods {..} -> get state

c1 :: Counter
c1 = c & \case
  Counter {..} -> methods & \case
    Methods {..} ->
      Counter
        { state = inc state
        , methods = methods
        }

sendInc :: Counter -> Counter
sendInc = \case
  Counter {..} -> methods & \case
    Methods {..} ->
      Counter
        { state = inc state
        , methods = methods
        }

-- λ> sendGet . add3 $ c
-- 8
add3 :: Counter -> Counter
add3 = sendInc . sendInc . sendInc