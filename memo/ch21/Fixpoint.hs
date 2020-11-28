module Fix where

import Data.Set (Set)
import qualified Data.Set as Set

type UnivSet = Set String
type InferRule = [(UnivSet, UnivSet)]
type Function = Set (UnivSet, UnivSet)

u1 :: UnivSet
u1 = Set.fromList ["a", "b", "c"]

ruleE1 :: InferRule
ruleE1 =
  [ (Set.empty, Set.fromList ["c"])
  , (Set.fromList ["c"], Set.fromList ["b"])
  , (Set.fromList ["b", "c"], Set.fromList ["a"])
  ]

e1 :: Function
e1 = genFunction u1 ruleE1

ruleE2 :: InferRule
ruleE2 =
  [ (Set.empty, Set.fromList ["a"])
  , (Set.fromList ["c"], Set.fromList ["b"])
  , (Set.fromList ["a", "b"], Set.fromList ["c"])
  ]

e2 :: Function
e2 = genFunction u1 ruleE2

-- ex 21.5.4
u2 :: UnivSet
u2 = Set.fromList ["a", "b", "c", "d", "e", "f", "g", "h", "i"]

ruleE3 :: InferRule
ruleE3 = 
  [ (Set.empty, Set.fromList ["g"])
  , (Set.fromList ["g"], Set.fromList ["f"])
  , (Set.fromList ["f", "g"], Set.fromList ["c"])
  , (Set.fromList ["e"], Set.fromList ["b"])
  , (Set.fromList ["d"], Set.fromList ["e"])
  , (Set.fromList ["b"], Set.fromList ["d"])
  , (Set.fromList ["b", "c"], Set.fromList ["a"])
  , (Set.fromList ["i", "a"], Set.fromList ["h"])
  ]

{-
λ> Set.size e3
512

λ> closedSet e3
fromList
  [ fromList ["a","b","c","d","e","f","g"]
  , fromList ["a","b","c","d","e","f","g","h"]
  , fromList ["a","b","c","d","e","f","g","h","i"]
  , fromList ["a","c","f","g"]
  , fromList ["a","c","f","g","h"]
  , fromList ["a","c","f","g","h","i"]
  , fromList ["c","f","g"]
  , fromList ["c","f","g","h"]
  , fromList ["c","f","g","h","i"]
  , fromList ["c","f","g","i"]
  ]

λ> consistentSet e3
fromList
  [ fromList []
  , fromList ["a","b","c","d","e","f","g"]
  , fromList ["b","c","d","e","f","g"]
  , fromList ["b","d","e"]
  , fromList ["b","d","e","f","g"]
  , fromList ["b","d","e","g"]
  , fromList ["c","f","g"]
  , fromList ["f","g"]
  , fromList ["g"]
  ]

λ> fixpointSet e3
fromList
  [ fromList ["a","b","c","d","e","f","g"]
  , fromList ["c","f","g"]
  ]
-}

e3 :: Function
e3 = genFunction u2 ruleE3

-- helper
genFunction :: UnivSet -> InferRule -> Function
genFunction uSet rule = Set.map gen pSet
  where
    pSet = Set.powerSet uSet
    gen s = (s, foldr Set.union Set.empty [ c | (d,c) <- rule, d `Set.isSubsetOf` s])

closedSet :: Function -> Set (Set String)
closedSet = satSet isClosed

consistentSet :: Function -> Set (Set String)
consistentSet = satSet isConsistent

fixpointSet :: Function -> Set (Set String)
fixpointSet = satSet isFixpoint

lfpSet :: Function -> Set String
lfpSet = foldr1 Set.intersection . Set.toList . closedSet

gfpSet :: Function -> Set String
gfpSet = Set.unions . consistentSet

isClosed :: Ord a => (Set a, Set a) -> Bool
isClosed (s1,s2) = s2 `Set.isSubsetOf` s1

isConsistent :: Ord a => (Set a, Set a) -> Bool
isConsistent (s1,s2) = s1 `Set.isSubsetOf` s2

isFixpoint :: Ord a => (Set a, Set a) -> Bool
isFixpoint s = isClosed s && isConsistent s

satSet :: Ord b1 => ((b1, b2) -> Bool) -> Set (b1, b2) -> Set b1
satSet p = Set.map fst . Set.filter p