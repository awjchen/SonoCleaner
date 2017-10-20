module Model.Util
  ( estimateSlope
  ) where

import           Control.Monad.ST
import qualified Data.IntMap.Strict           as M
import           Data.Vector.Algorithms.Intro (partialSort)
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Unboxed.Mutable  as VM
import           Statistics.Test.Types
import           Statistics.Test.WilcoxonT    (wilcoxonMatchedPairTest)
import           Statistics.Types

estimateSlope :: V.Vector Double -> M.IntMap Double -> Int -> Int -> Maybe Double
estimateSlope ds jumps radius i
  | V.length nonZeroSlopes < 1 = Nothing
  | pValue' < 0.10 = Just (median slopes)
  | otherwise      = Just 0
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
median v =
  let n = V.length v
      n2 = n `quot` 2 + 1
      avg x y = (x+y)/2
  in  runST $ do
        mv <- V.thaw v
        partialSort mv n2
        if odd n
          then VM.read mv n2
          else avg <$> VM.read mv (pred n2) <*> VM.read mv n2
