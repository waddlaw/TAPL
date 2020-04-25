{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

data Stack where
  Stack :: forall a.
    { new     :: a
    , push    :: Int -> a -> a
    , top     :: a -> Int
    , pop     :: a -> a
    , isEmpty :: a -> Bool
    } -> Stack

stackADT :: Stack
stackADT = Stack
  { new     = []
  , push    = (:)
  , top     = head
  , pop     = tail
  , isEmpty = null
  }

-- λ> stackEx
-- 2
stackEx :: Int
stackEx =
  case stackADT of
    Stack {..} -> top . push 2 . push 4 . push 8 . push 10 $ new

{-
stackADT =
  {*List Nat,
    { new  = nil [Nat]
    , push = λn:Nat. λs:List Nat. cons [Nat] n s
    , top  = λs:List Nat. head [Nat] s
    , pop  = λs:List Nat. tail [Nat] s
    , isempty = isnil [Nat]
    }
  } as {∃Stack, {new:Stack, push:Nat->Stack->Stack, top:Stack->Nat, pop:Stack->Stack,isempty:Stack->Bool}}
-}