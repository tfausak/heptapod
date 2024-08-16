import qualified Data.Time as Time
import qualified Data.Time.Clock.System as Time
import qualified Data.UUID.Types as Uuid
import qualified Heptapod
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree =
  Tasty.testGroup
    "Heptapod"
    [ Tasty.testGroup
        "generate"
        [ Unit.testCase "works" $ do
            uuid <- Heptapod.generate
            length (Uuid.toString uuid) Unit.@?= 36
        ],
      Tasty.testGroup
        "build"
        [ Unit.testCase "works with nil" $ do
            let epoch = Time.MkSystemTime 0 0
            let uuid = Heptapod.build epoch 0 0
            Uuid.toString uuid Unit.@?= "00000000-0000-7000-8000-000000000000",
          Unit.testCase "works with test vector" $ do
            -- https://datatracker.ietf.org/doc/html/rfc9562#name-example-of-a-uuidv7-value
            let time =
                  Time.utcToSystemTime
                    . Time.localTimeToUTC (Time.hoursToTimeZone (-5))
                    . Time.LocalTime (Time.fromGregorian 2022 2 22)
                    $ Time.TimeOfDay 14 22 22
            let uuid = Heptapod.build time 0xcc3 0x18C4DC0C0C07398F
            Uuid.toString uuid Unit.@?= "017f22e2-79b0-7cc3-98c4-dc0c0c07398f"
        ]
    ]
