module Data.Fixed.Decimal.Int128
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Int128)
import Data.Fixed.Decimal


type Decimal s = DecimalP Int128 s
