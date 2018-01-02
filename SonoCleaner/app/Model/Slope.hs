module Model.Slope
  ( estimateSlope
  , median
  ) where

import qualified Data.IntMap.Strict           as M
import           Data.Vector.Algorithms.Intro (partialSort)
import qualified Data.Vector.Unboxed          as V
import           Statistics.Test.Types
import           Statistics.Test.WilcoxonT    (wilcoxonMatchedPairTest)
import           Statistics.Types

estimateSlope :: V.Vector Double -> M.IntMap Double -> Int -> Int -> Double
estimateSlope ds jumps radius i
  | V.length nonZeroSlopes < 5 = 0 -- p < 0.10 impossible with l.t. 5 slopes
  | pValue' < 0.10             = median slopes
  | otherwise                  = 0
  where
    i0 = max 0                 (i-radius)
    i1 = min (V.length ds - 1) (i+radius)
    run = i1-i0+1
    slopes = V.map snd
           $ V.filter (not . flip M.member jumps . fst)
           $ V.zip (V.generate run (+i0))
           $ V.slice i0 run ds
    nonZeroSlopes = V.filter (/= 0) slopes
    pairs = V.zip nonZeroSlopes (V.replicate (V.length nonZeroSlopes) 0)
    pValue' = pValue $ testSignificance
            $ wilcoxonMatchedPairTest SamplesDiffer pairs

-- Fails if vector is empty
median :: V.Vector Double -> Double
median v
  | odd n = v' V.! middleIndex
  | otherwise = avg (v' V.! pred middleIndex) (v' V.! middleIndex)
  where
    n = V.length v
    middle = n `quot` 2 + 1
    middleIndex = pred middle
    v' = V.modify (`partialSort` middle) v
    avg x y = (x+y)/2
