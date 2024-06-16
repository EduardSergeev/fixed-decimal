{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Fixed.Decimal.Class
(
    FixedDecimal(..)
) where

import Data.Kind (Type)
import GHC.TypeLits (Nat)

-- | Fixed decimal types class
class FixedDecimal d where
    -- | 'Integral' type for storing mantissa 
    type Precision d :: Type
    -- | The size of the fractional part in decimal digits
    type Scale d :: Nat
    -- | Runtime value of @Scale d@
    scale :: d -> Int
    -- | Value constructor
    --
    -- @decimal m e@ constructs a decimal number via @m * 10 '^^' e@.
    decimal :: Integral m => m -> Int -> d
