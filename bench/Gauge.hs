import Gauge
import qualified Term.HashSet
import qualified Term.Set

main :: IO ()
main =
  defaultMain
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
    ]
