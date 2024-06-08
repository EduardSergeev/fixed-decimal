{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Fixed.Decimal.Class
(
    FixedDecimal(..)
) where

import Data.Kind (Type)
import GHC.TypeLits (Nat)


class FixedDecimal d where
    type Precision d :: Type
    type Scale d :: Nat
    scale :: d -> Int
    decimal :: Integral i => i -> Int -> d
