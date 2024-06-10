{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

import           Control.Arrow ((&&&))
import           Control.DeepSeq (NFData)
import qualified Data.Decimal as DD
import           Data.DoubleWord
import qualified Data.Fixed.Decimal as FD
import           Data.List (foldl')
import           Test.Tasty.Bench
import           Text.Printf (printf)



benchmarks :: [Benchmark]
benchmarks =
    fmap (uncurry benchmark . (show &&& id))  [
        1000
    ]

benchmark :: String -> Int -> Benchmark
benchmark t n =
    bgroup t [
        bgroup "Data.Fixed.Decimal" [
            group "Decimal Int128 10" n (0.001 :: FD.Decimal Int128 10),
            group "Decimal Int256 10" n (0.001 :: FD.Decimal Int256 10)
        ],
        group "Data.Decimal" n (0.001 :: DD.Decimal),
        group "Double" n (0.001 :: Double)
    ]

group :: (NFData a, Fractional a) => String -> Int -> a -> Benchmark
group t n d =
    bgroup t [
        bdef "+" $
            nf (\n' -> sum $ replicate n' d) n,
        bdef "-" $
            nf (\n' -> foldl' (-) (fromIntegral n' * d) $ replicate n' d) n,
        bdef "*" $
            nf (\n' -> product $ replicate n' d) n,
        bdef "/" $        
            nf (\n' -> foldl' (/) (d ^ n') $ replicate n' d) n
    ]
    where
        bdef o =
            bench $ printf "%s (%dx times)" o n


deriving instance NFData p => NFData (FD.Decimal p s)

instance NFData Int256 where

instance NFData Int128 where

instance NFData Word128 where


main :: IO ()
main =
    defaultMain benchmarks
