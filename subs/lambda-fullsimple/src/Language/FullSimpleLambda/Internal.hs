module Language.FullSimpleLambda.Internal
  ( isValue
  , isNumericValue
  , isRecordValue
  ) where

import RIO
import           Language.FullSimpleLambda.Types

-- | 与えられた項が値かどうか判定する述語
isValue :: Term -> Bool
isValue TmVar{}        = True
isValue TmLam{}        = True
isValue TmTrue         = True
isValue TmFalse        = True
isValue TmUnit         = True                     -- 11.2 Unit型
isValue (TmPair t1 t2) = isValue t1 && isValue t2 -- 11.6 2つ組
isValue (TmTuple ts)   = all isValue ts           -- 11.7 組
isValue (TmRecord fs)  = all (isValue . snd) fs   -- 11.8 レコード
isValue t              = isNumericValue t

-- | 与えられた項が数項かどうか判定
isNumericValue :: Term -> Bool
isNumericValue TmZero     = True
isNumericValue (TmSucc t) = isNumericValue t
isNumericValue _          = False

-- | 与えられた項がレコードかつ、値かどうか判定
isRecordValue :: Term -> Bool
isRecordValue t@TmRecord{} = isValue t
isRecordValue _            = False
