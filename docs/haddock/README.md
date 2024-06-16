# fixed-decimal

[![Build Status](https://github.com/EduardSergeev/fixed-decimal/actions/workflows/master.yml/badge.svg?branch=master)](https://github.com/EduardSergeev/fixed-decimal/actions?query=workflow%3Amaster+branch%3Amaster)
[![Test Coverage](https://coveralls.io/repos/github/EduardSergeev/fixed-decimal/badge.svg)](https://coveralls.io/github/EduardSergeev/fixed-decimal)
[![Documentation](https://eduardsergeev.github.io/fixed-decimal/haddock.svg)](https://eduardsergeev.github.io/fixed-decimal/haddock/)

[Fixed-precision decimals](https://en.wikipedia.org/wiki/Fixed-point_arithmetic) for Haskell

# How to use it

The core of this library is type `Decimal (m :: Type) (s :: Nat)` which expects two arguments:

- Integral `m :: Type` to store mantissa:  
  Any `Integral` type can be passed as `m` parameter: standard `Int`, [double-word](https://hackage.haskell.org/package/data-dword)'s signed `Int256` or unsigned `Word128` or even `Integer` for mantissa or arbitrary length;
- Type-level number `s :: Nat` which specifies the fractional part size (in decimal digits).

The following example defines decimal number type which uses (signed) 256 bits to store mantissa with 10-digit fractional part:

```haskell
{-# LANGUAGE DataKinds #-}

import Data.DoubleWord (Int256)
import Data.Fixed.Decimal as (Decimal)

type DecimalI256 = Decimal Int256 10
```

Minimum and maximum values which can be stored for various `Decimal m s` flavours:

```haskell
>>> minBound :: Decimal Int 5
-92233720368547.75808

>>> maxBound :: Decimal Int256 25
5789604461865809771178549250434395392663499233282028.2019728792003956564819967

>>> minBound :: Decimal Word128 10
0

>>> maxBound :: Decimal Word128 10
34028236692093846346337460743.1768211455
```

To use mantissa of arbitrary length use `Integer`:

```haskell
>>> 1 / 3 :: Decimal Integer 50
0.33333333333333333333333333333333333333333333333333
```

# Why

In comparison to floating-point numeric data types like `Float` or `Double` fixed-precision decimals can store decimal numbers with exact precision.  

For example `Double` cannot represent number `0.01` exactly:

```haskell
>>> sum $ replicate 10 (0.01 :: Double)
9.999999999999999e-2

>>> sum $ replicate 100 (0.01 :: Double)
1.0000000000000007
```

while `Decimal m s` can:

```haskell
>>> sum $ replicate 10 (0.01 :: Decimal Int 2)
0.1

>>> sum $ replicate 100 (0.01 :: Decimal Int 2)
1
```

# Overflows and performance

In spirit of other Haskell numeric types `Decimal m s` does cannot detect of handle numeric overflows, i.e. when the result of operation cannot be represented using the supplied `m :: Type` mantissa type or `s :: Nat` digits of fractional part. In such case no error will be thrown while the resulting number will be incorrect.  
The only way to avoid this situation is to select appropriate `m` and `s`: large enough to store any possible results.  
NB: Smaller `m :: Type` however exhibit better performance, e.g. `Decimal Int 5` will be more performant that `Decimal Int256 5` and much better then `Decimal Integer 5`


# Benchmarks

[![Linux benchmarks](https://eduardsergeev.github.io/fixed-decimal/bench-linux.svg)](https://eduardsergeev.github.io/fixed-decimal/ubuntu-latest/results.html)
[![Windows benchmarks](https://eduardsergeev.github.io/fixed-decimal/bench-windows.svg)](https://eduardsergeev.github.io/fixed-decimal/windows-latest/results.html)
[![MacOS benchmarks](https://eduardsergeev.github.io/fixed-decimal/bench-macos.svg)](https://eduardsergeev.github.io/fixed-decimal/macos-latest/results.html)

Benchmarks for various flavours of `Decimal m s` plus the results for the same benchmarks for `Double` and [Decimal](https://hackage.haskell.org/package/Decimal)
