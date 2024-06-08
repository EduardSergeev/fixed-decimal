module Data.Fixed.Decimal.Int256
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Int256)
import Data.Fixed.Decimal


type Decimal s = DecimalP Int256 s
