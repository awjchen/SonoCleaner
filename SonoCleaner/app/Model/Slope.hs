module Model.Slope
  ( estimateSlope
  , median
  ) where

import           Data.Vector.Algorithms.Intro (partialSort)
import qualified Data.Vector.Unboxed          as V
import           Statistics.Test.Types
import           Statistics.Test.WilcoxonT    (wilcoxonMatchedPairTest)
import           Statistics.Types

import           Types.Indices

estimateSlope
  :: IVector Index1 Double
  -> IIntMap Index1 Double
  -> Index1
  -> Index1
  -> Double
estimateSlope ds jumps radius i
  | V.length nonZeroSlopes < 5 = 0 -- p < 0.10 impossible with l.t. 5 slopes
  | pValue' < 0.10             = median slopes
  | otherwise                  = 0
  where
    i0 = max 0                 (i-radius)
    i1 = min (ivLength ds - 1) (i+radius)
    -- run = i1-i0+1
    slopes = V.fromList $ map (ivIndex ds)
                        $ filter (not . flip iimMember jumps) [i0..i1]
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
