{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

import qualified Data.Decimal as DD
import           Data.Scientific (Scientific(..), scientific)
import           Fixed.Decimal
import           Test.Hspec (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Scientific ()


instance Arbitrary DD.Decimal where
    arbitrary = flip suchThat (\d -> let sd = show d in '.' `notElem` sd || last sd /= '0') $ do
        m <- chooseInteger (-10 ^ (15 :: Int), 10 ^ (15 :: Int))
        e <- choose (0, fromIntegral precision)
        pure (DD.Decimal e m)


spec :: Spec
spec = do
    describe "Decimal" $ do
        describe "Behaviour equivalence test" $ do
            modifyMaxSuccess (const 1000) $ do
                it "Scientific roundtrip" $ do
                    property prop_sientificEq
                it "Rational roundtrip" $ do
                    property prop_rationalEq
                it "DD.Decimal equivalence in terms of addition" $ do
                    property prop_decimalPlusEq
                it "Precision test" $ do
                    property prop_precision
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

prop_decimalPlusEq :: Scientific -> Scientific -> Property
prop_decimalPlusEq sl sr =
    inRange sl && inRange sr ==> toRational (el + er) === toRational (dl + dr) 
    where
        l = toRational sl
        r = toRational sr
        el = fromRational @DD.Decimal l
        er = fromRational @DD.Decimal r
        dl = fromRational @Decimal l
        dr = fromRational @Decimal r

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
    base10Exponent s > -precision &&
    base10Exponent s < precision - 1


toScientific :: Decimal -> Scientific
toScientific (Decimal m) =
    scientific (fromIntegral m) (negate precision)

fromScientific :: Scientific -> Decimal
fromScientific s =
    decimal (coefficient s) (base10Exponent s)


main :: IO ()
main =
    hspec spec      
