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


testFor :: forall m s. (Integral m, Typeable m, KnownNat s, KnownNat (10 ^ s), Show m, Read m) => Decimal m s -> Decimal m s -> TestTree
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
                    pure $ show (abs $ fromDecimal @DD.Decimal d) === show (abs d),
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
            ],
            testGroup "Read tests" [
                testProperty "read . show"
                    prop_showReadRoundtrip,
                testInput "1." $ \s ->
                    reads s @?= [(1 :: Decimal m s, "")],
                testInput "1.1.1" $ \s ->
                    reads s @?= [(1.1 :: Decimal m s, ".1")],
                testInput "123$" $ \s ->
                    reads s @?= [(123 :: Decimal m s, "$")]
            ]        
        ],
        testGroup "Specific Show cases" [
            testInput "0" $ \s ->
                show (0 :: Decimal m s) @?= s,
            testInput "42" $ \s ->
                show (42 :: Decimal m s) @?= s,
            testInput "42.1" $ \s ->
                show (42.1 :: Decimal m s) @?= s,
            testInput "-42.01" $ \s ->
                show (-42.01 :: Decimal m s) @?= s,
            testInput "-42.00042" $ \s ->
                show (-42.00042 :: Decimal m s) @?= s
        ]
    ]
    where
        gen :: Gen (Decimal m s)
        gen = do
            gw <- oneof [sw, lw]
            gf <- oneof [sf, lf]
            w <- elements [0, gw]
            f <- elements [0, gf]
            let d = read @(Decimal m s) $ show w ++ "." ++ show f
            pure d
            where
                s = scale (undefined :: Decimal m s)
                p = ceiling @Double @Int . logBase 10 . fromIntegral . mantissa $ dmax
                wmax = 10 ^ (p `quot` 2 - s)
                wmin = negate wmax
                fmax = 10 ^ (s `quot` 2)
                fmin = 0
                sw = (toInteger <$> arbitrary @(Small Integer)) `suchThat` \i -> i > wmin && i < wmax
                lw = chooseInteger (wmin, wmax)
                sf = (toInteger <$> arbitrary @(Small Integer)) `suchThat` \i -> i > fmin && i < fmax
                lf = chooseInteger (fmin, fmax)

        testInput s t =
            testCase (show s) $ t s

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

        prop_showReadRoundtrip = do
            d <- gen
            pure $ d === (read . show) d

        toScientific :: Decimal m s -> Scientific
        toScientific d =
            scientific (fromIntegral . mantissa $ d) (negate . scale $ d)

        fromScientific :: Scientific -> Decimal m s
        fromScientific s =
            decimal (coefficient s) (base10Exponent s)

        fromDecimal :: Fractional a => Decimal m s -> a
        fromDecimal =
            fromRational . toRational
