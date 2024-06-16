{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fixed.Decimal.Tests
(
    testFor
) where

import           Control.Arrow ((***))
import qualified Data.Decimal as DD
import           Data.Fixed.Decimal
import           Data.Scientific (Scientific(..), scientific)
import           Data.Typeable (Typeable, typeOf)
import           GHC.TypeLits (KnownNat, type (^))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))
import           Test.Tasty.QuickCheck hiding (scale)


testFor :: forall m s. (Integral m, Typeable m, KnownNat s, KnownNat (10 ^ s), Show m) => Decimal m s -> Decimal m s -> TestTree
testFor dmin dmax =
    testGroup (show $ typeOf dmin) [
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
                testProperty "show" $ do
                    d <- gen
                    pure $ show (fromDecimal @DD.Decimal d) === show d,
                testProperty "abs" $ do
                    d <- gen
                    pure $ show (fromDecimal @DD.Decimal d) === show (abs d),
                testProperty "signum" $ do
                    d <- gen
                    pure $ (show . signum @DD.Decimal . fromDecimal) d === (show . signum) d
        ],
        testGroup "Enum tests" [
                testProperty "fromEnum"
                    prop_fromEnum,
                testProperty "toEnum"
                    prop_toEnum,
                testProperty "enumFromTo"
                    prop_enumFromTo,
                testProperty "enumFromThenTo"
                    prop_enumFromThenTo
        ]
    ],
    testGroup "Specific Show cases" [
        testCase "0" $
            show (0 :: Decimal m s) @?= "0",
        testCase "42" $
            show (42 :: Decimal m s) @?= "42",
        testCase "42.1" $
            show (42.1 :: Decimal m s) @?= "42.1",
        testCase "-42.01" $
            show (-42.01 :: Decimal m s) @?= "-42.01",
      testCase "-42.00042" $
          show (-42.00042 :: Decimal m s) @?= "-42.00042"
    ]
  ]
    where
        gen :: Gen (Decimal m s)
        gen = do
            r <- Decimal . (* (10 ^ ((s `div` 2 + 1)))) . fromInteger <$> chooseInteger (minLim, maxLim)
            pure r
            where
                s = scale dmax
                p = floor @Double @Int . logBase 10 . fromIntegral . mantissa $ dmax
                minLim = toInteger $ mantissa dmin `mod` (10 ^ (p `div` 3))        
                maxLim = toInteger $ mantissa dmax `mod` (10 ^ (p `div` 3))        

        prop_sientificEq = do
            d <- gen
            pure $ d === (fromScientific . toScientific) d

        prop_rationalEq = do
            d <- gen
            pure $ d === (fromRational @(Decimal m s) . toRational $ d)

        prop_divisionMultiplication = do
            a <- gen
            b <- gen
            pure $ a /= 0 && b /= 0 ==> (a * b / a === b) .&&. (a * b / b === a)

        prop_decimalPlusEq = do
            dl <- gen
            dr <- gen
            let l = toRational dl
                r = toRational dr
                el = fromRational @DD.Decimal l
                er = fromRational @DD.Decimal r
            pure $ toRational (el + er) === toRational (dl + dr) 
            where

        prop_precision :: (Positive Int) -> (Positive Int) -> Property
        prop_precision (Positive n) (Positive di) =
            let d = fromIntegral @_ @Double di / 1000.0
                de = fromIntegral @_ @(Decimal m s) di / 1000
            in viaSum d /= (fromIntegral n) * d ==> viaSum de === (fromIntegral n) * de 
            where
                viaSum d = sum $ replicate n d

        prop_fromEnum = do
            fd <- gen
            let i = read . takeWhile (/= '.') . show $ fd
            pure $ i === fromEnum fd

        prop_toEnum i =
            let d = toEnum @Double i
                fd = toEnum @(Decimal m s) i
            in d === (read . show) fd

        prop_enumFromTo = do
            d1 <- gen
            d2 <- gen
            let dds = [fromDecimal @DD.Decimal d1 .. fromDecimal @DD.Decimal d2]
                fds = [d1 .. d2]
                (ds, fs) = unzip . filter (\(sd, _) -> last sd /= '0') . fmap (show *** show) . take 10 $ zip dds fds
            pure $ ds === fs

        prop_enumFromThenTo = do
            d1 <- gen
            d2 <- gen
            d3 <- gen
            let dds = [fromDecimal @DD.Decimal d1, fromDecimal @DD.Decimal d2 .. fromDecimal @DD.Decimal d3]
                fds = [d1, d2 .. d3]
                (ds, fs) = unzip . filter (\(sd, _) -> last sd /= '0') . fmap (show *** show) . take 10 $ zip dds fds
            pure $ ds === fs

        toScientific :: Decimal m s -> Scientific
        toScientific d =
            scientific (fromIntegral . mantissa $ d) (negate . scale $ d)

        fromScientific :: Scientific -> Decimal m s
        fromScientific s =
            decimal (coefficient s) (base10Exponent s)

        fromDecimal :: Fractional a => Decimal m s -> a
        fromDecimal =
            fromRational . toRational
