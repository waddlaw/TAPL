module Language.SystemF.Internal
  ( isValue,
    isNumericValue
    )
where

import Language.SystemF.Types
import RIO

-- | 与えられた項が値かどうか判定する述語
isValue :: Term -> Bool
isValue TmTrue = True -- 3-1. 真
isValue TmFalse = True -- 3-1. 偽
isValue TmLam {} = True -- 9-1. ラムダ抽象値
isValue TmTypeLam {} = True -- 23-1. 型抽象値
isValue t = isNumericValue t -- 3-2. 数値

-- | 与えられた項が数項かどうか判定
isNumericValue :: Term -> Bool
isNumericValue TmZero = True -- 3-2. ゼロ
isNumericValue (TmSucc t) = isNumericValue t -- 3-2. 後者値
isNumericValue _ = False