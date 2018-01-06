-- Slope estimates

module Model.Slope
  ( estimateSlope
  , median
  , getSurroundingSlopes
  ) where

import           Data.Vector.Algorithms.Intro (partialSort)
import qualified Data.Vector.Unboxed          as V
import           Statistics.Test.Types
import           Statistics.Test.WilcoxonT    (wilcoxonMatchedPairTest)
import           Statistics.Types

import           Types.Indices

estimateSlope
  :: IVector Index1 Double
  -> IIntSet Index1
  -> Int
  -> Index1
  -> Double
estimateSlope ds badSegments radius i
  | V.length nonZeroSlopes < 5 = 0 -- p < 0.10 impossible with l.t. 5 slopes
  | pValue' < 0.10             = median slopes
  | otherwise                  = 0
  where
    slopes = V.fromList $ getSurroundingSlopes ds badSegments radius i
    nonZeroSlopes = V.filter (/= 0) slopes
    pairs = V.zip nonZeroSlopes (V.replicate (V.length nonZeroSlopes) 0)
    pValue' = pValue $ testSignificance
            $ wilcoxonMatchedPairTest SamplesDiffer pairs

-- Fails if vector is empty
median :: V.Vector Double -> Double
median v
  | odd n = v' V.! middleIndex
  | otherwise = mid (v' V.! pred middleIndex) (v' V.! middleIndex)
  where
    n = V.length v
    middle = n `quot` 2 + 1
    middleIndex = pred middle
    v' = V.modify (`partialSort` middle) v
    mid x y = (x+y)/2

getSurroundingSlopes
  :: IVector Index1 Double
  -> IIntSet Index1
  -> Int
  -> Index1
  -> [Double]
getSurroundingSlopes ds badSegments radius i =
  map (ivIndex ds) $ filter (not . flip iisMember badSegments) [i0..i1]
  where
    (i0, i1) = runIndexInterval $ iiBoundByIVector ds
             $ IndexInterval (translate (-radius) i, translate radius i)
