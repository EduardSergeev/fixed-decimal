import Data.DoubleWord (Int128, Int256)
import Data.Fixed.Decimal (Decimal)
import Data.Fixed.Decimal.Tests (testFor)
import Test.Tasty (TestTree, defaultMain, testGroup)


tests :: TestTree
tests =
  testGroup "Data.Fixed.Decimal" [
    testFor (minBound :: Decimal Int 5) maxBound,
    testFor (minBound :: Decimal Int128 10) maxBound,
    testFor (minBound :: Decimal Int256 25) maxBound,
    testFor
      (fromRational @(Decimal Integer 25) . toRational $ (minBound :: Decimal Int256 25))
      (fromRational @(Decimal Integer 25) . toRational $ (maxBound :: Decimal Int256 25))
  ]


main :: IO ()
main =
  defaultMain tests
