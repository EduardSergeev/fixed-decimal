module Data.Fixed.Decimal.Int160
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Int160)
import Data.Fixed.Decimal


type Decimal s = DecimalP Int160 s
