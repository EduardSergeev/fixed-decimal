module Data.Fixed.Decimal.Word128
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Word128)
import Data.Fixed.Decimal


type Decimal s = DecimalP Word128 s
