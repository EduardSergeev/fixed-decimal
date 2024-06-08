{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Fixed.Decimal
(
    FixedDecimal(..),
    DecimalP(..)
) where

import Data.Fixed.Decimal.Class
import Data.Kind (Type)
import Data.List (foldl')
import Data.Ratio (denominator, numerator, (%))
import GHC.TypeLits (KnownNat(..), Nat, natVal)


newtype DecimalP (p :: Type) (s :: Nat) = Decimal {
    mantissa :: p
} deriving (Eq, Ord)

instance (Integral m, KnownNat s) => FixedDecimal (DecimalP m s) where
    type Scale (DecimalP m s) = s
    type Precision (DecimalP m s) = m
    scale _ =
        fromInteger $ natVal @s undefined
    decimal m e =
        let ma = normalise $ fromIntegral m * (10 ^ (scale @(DecimalP m s) undefined + e + 1))
        in Decimal ma

instance (Show m, Integral m, KnownNat s) => Show (DecimalP m s) where
    show (Decimal 0) =
        "0"
    show dm@(Decimal m) =
        fst . foldl' step ([], scale dm) . pad (scale dm + 1) . reverse . show $ m
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


instance (Integral m, KnownNat s) => Num (DecimalP m s) where
    (Decimal l) + (Decimal r) =
        Decimal $ l + r

    (Decimal l) - (Decimal r) =
        Decimal $ l - r

    (Decimal l) * d@(Decimal r) =
        Decimal . normalise $ l * r `div` (10 ^ (scale d - 1))

    abs (Decimal m) =
        Decimal $ abs m

    signum d@(Decimal m) =
        Decimal $ (10 ^ scale d) * signum m

    fromInteger i =
        Decimal $ (10 ^ scale (undefined :: DecimalP m s)) * fromInteger i


instance (Integral m, KnownNat s) => Fractional (DecimalP m s) where
  fromRational r =
    fromInteger (numerator r) / fromInteger (denominator r)

  (Decimal l) / d@(Decimal r) =
    Decimal . normalise $ (10 ^ (scale d + 1)) * l `div` r
    
instance (Bounded m, Integral m) => Bounded (DecimalP m s) where
    minBound =
        Decimal . normalise $ (minBound :: m)
    maxBound =
        Decimal . normalise $ (maxBound :: m)


instance (Integral m, KnownNat s) => Real (DecimalP m s) where
    toRational d@(Decimal m) =
        fromIntegral m % 10 ^ scale d