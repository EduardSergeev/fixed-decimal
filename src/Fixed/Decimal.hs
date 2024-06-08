{-# LANGUAGE TypeApplications #-}

module Fixed.Decimal
(
    Decimal(..),
    decimal,
    precision
) where

import Data.DoubleWord
import Data.List (foldl')
import Data.Ratio (denominator, numerator, (%))


type MantissaType = Int256

newtype Decimal = Decimal {
    _mantissa :: MantissaType
} deriving (Eq, Ord)

precision :: Int
precision = 25

decimal :: Integer -> Int -> Decimal
decimal m e =
    let ma = normalise $ fromInteger m * (10 ^ (precision + e + 1))
    in Decimal ma


instance Show Decimal where
    show (Decimal 0) =
        "0"
    show (Decimal m) =
        fst . foldl' step ([], precision) . pad (precision + 1) . reverse . show $ m
        where
            step ([], i) '0' | i > 0 =
                ([], i - 1)
            step (ds, i) d | i > 0 =
                (d : ds, i - 1)
            step ([], 0) d =
                ([d], -1)
            step (ds, 0) d =
                (d : '.' : ds, -1)
            step (ds, i) d  =
                (d : ds, i - 1)
            pad 0 ds =
                ds
            pad i [] =
                '0' : pad (pred i) []
            pad i "-" =
                '0' : pad (pred i) "-"
            pad i (d : ds) =
                d : pad (pred i) ds

normalise :: Integral a => a -> a
normalise i =
    case i `quotRem` 10 of
        (q, r) | abs r < 5 -> q
        (q, _) | q > 0 -> succ q
        (q, _) -> pred q


instance Num Decimal where
    (Decimal l) + (Decimal r) =
        Decimal $ l + r

    (Decimal l) - (Decimal r) =
        Decimal $ l - r

    (Decimal l) * (Decimal r) =
        Decimal . normalise $ l * r `div` (10 ^ (precision - 1))

    abs (Decimal m) =
        Decimal $ abs m

    signum (Decimal m) =
        Decimal $ (10 ^ precision) * signum m

    fromInteger i =
        Decimal $ (10 ^ precision) * fromInteger i


instance Fractional Decimal where
  fromRational r =
    fromInteger (numerator r) / fromInteger (denominator r)

  (Decimal l) / (Decimal r) =
    Decimal . normalise $ (10 ^ (precision + 1)) * l `div` r
    
instance Bounded Decimal where
    minBound =
        Decimal . normalise $ (minBound :: MantissaType)
    maxBound =
        Decimal . normalise $ (maxBound :: MantissaType)


instance Real Decimal where
    toRational (Decimal m) =
        fromIntegral m % 10 ^ precision
