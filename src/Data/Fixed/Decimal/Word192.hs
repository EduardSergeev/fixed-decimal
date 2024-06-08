module Data.Fixed.Decimal.Word192
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Word192)
import Data.Fixed.Decimal


type Decimal s = DecimalP Word192 s
