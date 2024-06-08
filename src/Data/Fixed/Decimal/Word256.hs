module Data.Fixed.Decimal.Word256
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Word256)
import Data.Fixed.Decimal


type Decimal s = DecimalP Word256 s
