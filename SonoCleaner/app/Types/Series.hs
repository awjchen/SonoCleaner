-- Common utility functions for working with sequences of doubles

module Types.Series where
-- module Types.Series
--   ( diff
--   , undiff
--   , extend
--   , extendDiff
--   , extendDiff2
--   , contract

--   , unboxedAverage
--   , unboxedMinAndMax
--   ) where

-- import qualified Data.Vector.Unboxed as V

-------------------------------------------------------------------------------
-- Series
-------------------------------------------------------------------------------

-- diff :: V.Vector Double -> (Double, V.Vector Double)
-- diff v = (V.head v, V.zipWith (-) (V.tail v) v)

-- undiff :: (Double, V.Vector Double) -> V.Vector Double
-- undiff (h, v) = V.scanl' (+) h v

-- extend :: Int -> V.Vector Double -> V.Vector Double
-- extend r v = V.concat [ V.replicate r (V.head v)
--                       , v
--                       , V.replicate r (V.last v) ]

-- extendDiff :: Int -> V.Vector Double -> V.Vector Double
-- extendDiff r dv = V.concat [ V.replicate r 0
--                            , dv
--                            , V.replicate r 0 ]

-- extendDiff2 :: Int -> V.Vector Double -> V.Vector Double
-- extendDiff2 r ddv = V.concat [ V.replicate (r-1) 0
--                              , V.singleton (V.head ddv)
--                              , ddv
--                              , V.singleton (negate $ V.last ddv)
--                              , V.replicate (r-1) 0 ]

-- contract :: Int -> V.Vector Double -> V.Vector Double
-- contract r v = V.slice r (V.length v - 2*r) v

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

-- unboxedAverage :: V.Vector Double -> Double
-- unboxedAverage = (/) <$> V.sum <*> fromIntegral . V.length

-- unboxedMinAndMax :: V.Vector Double -> (Double, Double)
-- unboxedMinAndMax xs = V.foldl' minMaxAcc (x1, x1) (V.tail xs)
--   where
--     x1 = V.head xs

--     minMaxAcc :: (Double, Double) -> Double -> (Double, Double)
--     minMaxAcc (minAcc, maxAcc) x =
--       let minAcc' = min x minAcc
--           maxAcc' = max x maxAcc
--       in  minAcc' `seq` maxAcc' `seq` (minAcc', maxAcc')
