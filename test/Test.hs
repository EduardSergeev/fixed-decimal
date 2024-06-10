{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Decimal as DD
import           Data.DoubleWord (Int256)
import           Data.Fixed.Decimal hiding (Decimal)
import qualified Data.Fixed.Decimal as D
import           Data.Scientific (Scientific(..), scientific)
import           Test.QuickCheck.Instances.Scientific ()
import           Test.Tasty (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck hiding (scale)


type FractionalPartPrecision = 15
type Decimal = D.Decimal Int256 FractionalPartPrecision

tests :: TestTree
tests =
  testGroup "Decimal tests" [
    testGroup "QuickCheck tests" [
      testGroup "Roundtrip tests" [
        testProperty "Scientific roundtrip"
          prop_sientificEq,
        testProperty "Rational roundtrip"
          prop_rationalEq,
        testProperty "Precision test"
          prop_precision  
      ],
      testGroup "Behaviour equivalence tests" [
        testProperty "/ * test"
          prop_divisionMultiplication,
        testProperty "DD.Decimal equivalence in terms of addition"
          prop_decimalPlusEq,
        testProperty "show" $
          \e -> show @DD.Decimal e === (show . fromRational @Decimal . toRational) e,
        testProperty "abs" $
          \e -> (show . abs @DD.Decimal) e === (show . abs . fromRational @Decimal . toRational) e,
        testProperty "signum" $
          \e -> (show . signum @DD.Decimal) e === (show . signum . fromRational @Decimal . toRational) e
      ]
    ],
    testGroup "Specific Show cases" [
      testCase "0" $
          show (0 :: Decimal) @?= "0",
      testCase "42" $
          show (42 :: Decimal) @?= "42",
      testCase "42.1" $
          show (42.1 :: Decimal) @?= "42.1",
      testCase "-42.0000001" $
          show (-42.0000001 :: Decimal) @?= "-42.0000001",
      testCase "0.00000042" $
          show (0.00000042 :: Decimal) @?= "0.00000042",
      testCase "-0.00000042" $
          show (-0.00000042 :: Decimal) @?= "-0.00000042"
    ]
  ]

prop_sientificEq :: Scientific -> Property
prop_sientificEq s =
  inRange s ==> s === (toScientific . fromScientific) s

prop_rationalEq :: Scientific -> Property
prop_rationalEq s =
    inRange s ==> s === (fromRational . toRational . fromRational @Decimal . toRational) s

prop_divisionMultiplication :: Decimal -> Decimal -> Property
prop_divisionMultiplication a b =
    (a * b / a === b) .&&. (a * b / b === a)

prop_decimalPlusEq :: Decimal -> Decimal -> Property
prop_decimalPlusEq dl dr =
    toRational (el + er) === toRational (dl + dr) 
    where
        l = toRational dl
        r = toRational dr
        el = fromRational @DD.Decimal l
        er = fromRational @DD.Decimal r

prop_precision :: (Positive Int) -> (Positive Int) -> Property
prop_precision (Positive n) (Positive di) =
    let d = fromIntegral @_ @Double di / 1000.0
        de = fromIntegral @_ @Decimal di / 1000
    in viaSum d /= (fromIntegral n) * d ==> viaSum de === (fromIntegral n) * de 
    where
        viaSum d = sum $ replicate n d


inRange :: Scientific -> Bool
inRange s =
    s >= toScientific minBound &&
    s <= toScientific maxBound &&
    base10Exponent s > -lim &&
    base10Exponent s < lim - 1
    where
        lim = scale (undefined :: Decimal)

toScientific :: Decimal -> Scientific
toScientific d =
    scientific (fromIntegral . mantissa $ d) (negate . scale $ d)

fromScientific :: Scientific -> Decimal
fromScientific s =
    decimal (coefficient s) (base10Exponent s)


instance Arbitrary Decimal where
    arbitrary =
        D.Decimal . (* (10 ^ ((s `div` 2 + 1)))) <$> chooseBoundedIntegral (-lim, lim)
        where
            s = scale @Decimal undefined
            p = floor @Double @Int . logBase 10 . fromIntegral $ maxBound @(Precision Decimal)
            lim = maxBound @(Precision Decimal) `mod` (10 ^ (p `div` 3))

instance Arbitrary DD.Decimal where
    arbitrary = flip suchThat (\d -> let sd = show d in '.' `notElem` sd || last sd /= '0') $ do
        fromRational . toRational <$> arbitrary @Decimal


main :: IO ()
main =
  defaultMain tests
