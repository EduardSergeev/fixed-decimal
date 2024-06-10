{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Decimal as DD
import           Data.Fixed.Decimal
import qualified Data.Fixed.Decimal.Int256 as D
import           Data.Scientific (Scientific(..), scientific)
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck hiding (scale) --(Arbitrary(..), Property, Positive(..))
import           Test.QuickCheck.Instances.Scientific ()


type Decimal = D.Decimal 10

instance Arbitrary Decimal where
    arbitrary =
        Decimal <$> chooseBoundedIntegral (-lim, lim)
        where
            lim = (`div` 10) . floor @Double . sqrt . fromIntegral $ maxBound @(Precision Decimal)

instance Arbitrary DD.Decimal where
    arbitrary = flip suchThat (\d -> let sd = show d in '.' `notElem` sd || last sd /= '0') $ do
        m <- chooseInteger (-10 ^ (15 :: Int), 10 ^ (15 :: Int))
        e <- choose (0, fromIntegral . scale $ (undefined :: Decimal))
        pure $ DD.Decimal e m


spec :: Spec
spec = do
    describe "Decimal" $ do
        describe "QuickCheck tests" $ do
            modifyMaxSuccess (const 1000) $ do
                describe "Roundtrip tests" $ do
                    it "Scientific roundtrip" $ do
                        property prop_sientificEq
                    it "Rational roundtrip" $ do
                        property prop_rationalEq
                    it "Precision test" $ do
                        property prop_precision
                    it "/ * test" $ do
                        property prop_divisionMultiplication
                describe "Behaviour equivalence tests" $ do
                    it "DD.Decimal equivalence in terms of addition" $ do
                        property prop_decimalPlusEq
                    it "show" $ do
                        property $ \e -> show @DD.Decimal e === (show . fromRational @Decimal . toRational) e 
                    it "abs" $ do
                        property $ \e -> (show . abs @DD.Decimal) e === (show . abs . fromRational @Decimal . toRational) e 
                    it "signum" $ do
                        property $ \e -> (show . signum @DD.Decimal) e === (show . signum . fromRational @Decimal . toRational) e 
        describe "Specific Show cases" $ do
            it "0" $ do
                show (0 :: Decimal) `shouldBe` "0"
            it "42" $ do
                show (42 :: Decimal) `shouldBe` "42"
            it "42.1" $ do
                show (42.1 :: Decimal) `shouldBe` "42.1"
            it "-42.0000001" $ do
                show (-42.0000001 :: Decimal) `shouldBe` "-42.0000001"

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


main :: IO ()
main =
    hspec spec      
