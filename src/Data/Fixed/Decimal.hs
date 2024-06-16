{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Decimal types of fixed precision and scale
--   which can use any 'Integral' type to store mantissa
module Data.Fixed.Decimal
(
    module Data.Fixed.Decimal.Class,
    Decimal(..)
) where

import Data.Fixed.Decimal.Class
import Data.Kind (Type)
import Data.Ratio (denominator, numerator, (%))
import GHC.TypeLits (KnownNat, Nat, natVal, type (^))

-- | Decimal type of fixed precision and scale which uses:
--
--   * 'm': 'Integral' type to store mantissa
--   * 's': Type-level number 'Nat' to define scale (fractional part size)
--
newtype Decimal (m :: Type) (s :: Nat) = Decimal {
    mantissa :: m
} deriving (Eq, Ord)


instance (Integral m, KnownNat s) => FixedDecimal (Decimal m s) where
    type Scale (Decimal m s) = s
    type Precision (Decimal m s) = m
    scale _ =
        fromIntegral $ natVal @s undefined
    decimal m e =
        Decimal $ fromIntegral m * (10 ^ (scale @(Decimal m s) undefined + e))

type Scale10 (s :: Nat) = 10 ^ s

scale10 :: forall s i m. (KnownNat (10 ^ s), Integral i) => (Decimal m s) -> i
scale10 _ =
    fromInteger $ natVal @(Scale10 s) undefined


instance (Show m, Integral m, KnownNat s) => Show (Decimal m s) where
    show (Decimal 0) =
        "0"
    show dl@(Decimal dm) =
        let ds = gof (scale dl) (reverse . sabs . show $ dm) []
        in if dm < 0 then '-' : ds else ds
        where
            sabs ('-': ds) =
                ds
            sabs ds =
                ds

            gof 0 ds [] =
                goi ds []
            gof 0 ds rs =
                goi ds ('.' : rs)
            gof i ('0' : ds) [] =
                gof (pred i) ds [] 
            gof i (d : ds) rs =
                gof (pred i) ds (d : rs)
            gof i [] rs =
                gof (pred i) [] ('0' : rs)

            goi [] rs@('.' : _) =
                '0' : rs
            goi [] rs =
                rs
            goi (d : ds') rs =
                goi ds' (d : rs)

instance (Integral m, KnownNat s, KnownNat (10 ^ s)) => Num (Decimal m s) where
    (Decimal l) + (Decimal r) =
        Decimal $ l + r

    (Decimal l) - (Decimal r) =
        Decimal $ l - r

    (Decimal l) * d@(Decimal r) =
        Decimal $ l * r `div` scale10 d

    abs (Decimal m) =
        Decimal $ abs m

    signum d@(Decimal m) =
        Decimal $ scale10 d * signum m

    fromInteger i =
        Decimal $ scale10 (undefined :: Decimal m s) * fromInteger i


instance (Show m, Integral m, KnownNat s, KnownNat (10 ^ s)) => Fractional (Decimal m s) where
    fromRational r =
        fromInteger (numerator r) / fromInteger (denominator r)

    (Decimal l) / d@(Decimal r) =
        Decimal $ scale10 d * l `div` r
    
instance (Bounded m) => Bounded (Decimal m s) where
    minBound =
        Decimal $ minBound @m
    maxBound =
        Decimal $ maxBound @m


instance (Integral m, KnownNat s, KnownNat (10 ^ s)) => Real (Decimal m s) where
    toRational d@(Decimal m) =
        (fromIntegral m) % (scale10 d)

instance (Enum m, Integral m, KnownNat s, KnownNat (10 ^ s)) => Enum (Decimal m s) where
    fromEnum d@(Decimal m) =
        fromEnum $ m `div` (scale10  d)
    toEnum =
        fromIntegral
    enumFrom =
        iterate (+1)
    enumFromThen x1 x2 =
        let dx = x2 - x1 in iterate (+dx) x1
    enumFromTo x1 x2 =
        takeWhile (<= x2) $ enumFrom x1
    enumFromThenTo x1 x2 x3 =
        takeWhile (<= x3) $ enumFromThen x1 x2
