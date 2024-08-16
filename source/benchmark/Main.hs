import qualified Data.UUID.V1 as UUIDv1
import qualified Data.UUID.V4 as UUIDv4
import qualified Heptapod
import qualified Test.Tasty.Bench as Bench

main :: IO ()
main =
  Bench.defaultMain
    [ Bench.bgroup
        "uuid"
        [ Bench.bench "v1" $ Bench.nfIO UUIDv1.nextUUID,
          Bench.bench "v4" $ Bench.nfIO UUIDv4.nextRandom,
          Bench.bench "v7" $ Bench.nfIO Heptapod.generate
        ]
    ]
