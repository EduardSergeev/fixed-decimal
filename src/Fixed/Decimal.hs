{-# LANGUAGE TypeApplications #-}

module Fixed.Decimal
(
    Decimal(..),
    decimal,
    precision
) where

import Data.DoubleWord
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
    show (Decimal 0) = "0"
    show (Decimal m) =
        let n = length . show . abs $ m
            (q, r) = m `quotRem` (10 ^ precision)
            aq = abs q
            ar = abs r
            sign = if m < 0 then "-" else ""
            dot = if ar > 0 then "." else ""
        in concat [sign, show aq, dot, replicate (precision - n) '0', if ar > 0 then trim0 . show $ ar else ""]
        where
            trim0 = reverse . dropWhile (== '0') . reverse

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
        Decimal $ signum m

    fromInteger i =
        Decimal $ (10 ^ precision) * fromInteger i


instance Fractional Decimal where
  fromRational r =
    fromInteger (numerator r) / fromInteger (denominator r)

  (Decimal l) / (Decimal r) =
    Decimal . normalise $ (10 ^ (precision + 1)) * l `div` r
    
instance Bounded Decimal where
    minBound =
        Decimal . normalise $ (minBound :: MantissaType) -- `div` (10 ^ (precision - 1))
    maxBound =
        Decimal . normalise $ (maxBound :: MantissaType) -- `div` (10 ^ (precision - 1))


instance Real Decimal where
    toRational (Decimal m) =
        fromIntegral m % 10 ^ precision
