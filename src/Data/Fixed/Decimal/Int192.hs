module Data.Fixed.Decimal.Int192
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Int192)
import Data.Fixed.Decimal


type Decimal s = DecimalP Int192 s
