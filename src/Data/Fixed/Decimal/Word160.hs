module Data.Fixed.Decimal.Word160
(
    Decimal,
    module Data.Fixed.Decimal
) where

import Data.DoubleWord (Word160)
import Data.Fixed.Decimal


type Decimal s = DecimalP Word160 s
