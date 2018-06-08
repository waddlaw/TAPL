module KnuthBendix.Axiom
  ( groupAxioms
  ) where

import KnuthBendix.Types

-- | 群の公理
-- (1) 0 + x = x
-- (2) (-x) + x = 0
-- (3) (x + y) + z = x + (y + z)
groupAxioms :: Axioms
groupAxioms =
  [ Axiom (Func "+" [Func "0" [],vx]) vx
  , Axiom (Func "+" [Func "-" [vx],vx]) (Func "0" [])
  , Axiom (Func "+" [Func "+" [vx,vy],vz]) (Func "+" [vx,Func "+" [vy,vz]])
  ]
  where
    vx = Var "x"
    vy = Var "y"
    vz = Var "z"