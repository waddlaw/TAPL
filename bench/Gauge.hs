import           Gauge

import qualified Term.HashSet
import qualified Term.MonadSet
import qualified Term.Set

main :: IO ()
main = defaultMain
  [ bgroup "containers"
      [ bench "s 0" $ whnf Term.Set.s 0
      , bench "s 1" $ whnf Term.Set.s 1
      , bench "s 2" $ whnf Term.Set.s 2
      , bench "s 3" $ whnf Term.Set.s 3
      ]
  , bgroup "unorderd-containers"
      [ bench "s 0" $ whnf Term.HashSet.s 0
      , bench "s 1" $ whnf Term.HashSet.s 1
      , bench "s 2" $ whnf Term.HashSet.s 2
      , bench "s 3" $ whnf Term.HashSet.s 3
      ]
  , bgroup "set-monad"
      [ bench "s 0" $ whnf Term.MonadSet.s 0
      , bench "s 1" $ whnf Term.MonadSet.s 1
      , bench "s 2" $ whnf Term.MonadSet.s 2
      , bench "s 3" $ whnf Term.MonadSet.s 3
      , bench "s 4" $ whnf Term.MonadSet.s 4
      ]
  ]
