{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.DeepSeq (NFData, force)
import qualified Data.Decimal as DD
import           Data.DoubleWord (Int128, Int256, Word128, Word256)
import qualified Data.Fixed.Decimal as FD
import           Data.List (foldl')
import           Data.Ratio
import           Data.Typeable (Typeable, typeOf)
import           Data.Word (Word64)
import           Test.Tasty.Bench
import           Test.Tasty.Patterns.Printer (printAwkExpr)
import           Text.Printf (printf)


deriving instance NFData p => NFData (FD.Decimal p s)

instance NFData Int128 where
instance NFData Int256 where

instance NFData Word128 where
instance NFData Word256 where


benchmarks :: [Benchmark]
benchmarks = [
        benchmark 1000 (1.001)
    ]

benchmark :: Int -> Rational -> Benchmark
benchmark n r =
    bgroup tn [
        group (toD r :: Double),
        bgroup "Data.Fixed.Decimal" [
            bgroup "signed" [
                group @(FD.Decimal Int 5) (toD r),
                group @(FD.Decimal Int128 10) (toD r),
                group (toD r :: FD.Decimal Int256 25),
                group (toD r :: FD.Decimal Integer 50)
            ],
            bgroup "unsigned" [
                group (toD r :: FD.Decimal Word64 5),
                group (toD r :: FD.Decimal Word128 5),
                group (toD r :: FD.Decimal Word256 15)
            ]
        ],
        group (toD r :: DD.Decimal)
    ]
    where
        baseline = show . typeOf $ (toD r :: Double)
        tn = printf "%f x %d times" (fromRational @Double r) n

        toD :: Fractional d => Rational -> d
        toD = fromRational

        group :: forall d. (NFData d, Fractional d, Real d, Enum d, Typeable d, Show d) => d -> Benchmark
        group d =
            bgroup (show $ typeOf d) [
                bdef "+" $
                    nf sum ds,
                bdef "-" $
                    nf (foldl' (-) (fromIntegral n * d)) ds,
                bdef "*" $
                    nf product ds,
                bdef "/" $        
                    nf (foldl' (/) (d ^ n)) ds,
                bdef "abs" $
                    nf (fmap abs) fs,
                bdef "signum" $
                    nf (fmap signum) fs,
                bdef "fromRational" $
                    nf (fmap (fromRational @d)) fs,
                bdef "toRational" $
                    nf (fmap toRational) d2s,
                bdef "show" $
                    nf (fmap show) d2s
            ]
            where
                ds = force $ replicate n d
                fs = force [ (1 % dn) | dn <- [fromIntegral (-n) `div` 2 .. fromIntegral n `div` 2], dn /= 0 ]
                d2s = force [(d * fromIntegral n) .. (d * fromIntegral (n * n))]
                gn = show $ typeOf d
                bdef o
                    | gn == baseline =
                        bench bn
                    | otherwise = 
                        bcompare (printAwkExpr (locateBenchmark [bn, baseline, tn])) . (bench bn)
                    where
                        bn = printf "%s (%dx times)" o n :: String


main :: IO ()
main =
    defaultMain benchmarks
