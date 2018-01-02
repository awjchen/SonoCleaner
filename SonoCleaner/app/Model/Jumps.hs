-- Manual operations for eliminating jumps (level-shifts)

{-# LANGUAGE MultiWayIf #-}

module Model.Jumps
  ( zeroJump
  , estimateSlopeBoth

  , interpolateBetweenJumps
  , matchGroup
  ) where

import           Control.Applicative
import           Control.Lens        hiding (indices)
import qualified Data.IntMap.Strict  as M
import qualified Data.IntSet         as S
import           Data.Maybe          (fromMaybe)
import qualified Data.Vector.Unboxed as V

import qualified Types.IndexInterval as I
import           Types.LevelShifts
import           Types.Series

import           Model.TraceState
import           Model.Util

-------------------------------------------------------------------------------
-- Manual operatons on single jumps
-------------------------------------------------------------------------------

type SingleShift =
  V.Vector Double -> M.IntMap Double -> Int -> Double -> Maybe Double

liftToTrace ::
     SingleShift
  -> Hold
  -> Double
  -> Int -- Index of jump
  -> M.IntMap Double
  -> TraceStateOperator
liftToTrace f direction offset i jumps =
  unsafeTraceStateOperator $ \traceState ->
    fromMaybe traceState $ do
      x <- M.lookup i jumps -- jump height
      let ds = snd $ traceState ^. diffSeries
      x' <- (+offset) <$> f ds jumps i x -- new jump height
      let updates = [(i, x')]
          shift = case direction of
            HoldLeft  -> 0
            HoldRight -> x - x'
      return $ updateDiffSeries shift updates traceState

slopeEstimationRadius :: Int
slopeEstimationRadius = 4

zeroJump, estimateSlopeBoth
  :: Hold
  -> Double
  -> Int -- Index of jump
  -> M.IntMap Double
  -> TraceStateOperator
zeroJump           = liftToTrace  zeroJump'
estimateSlopeBoth  = liftToTrace (estimateSlopeBoth'  slopeEstimationRadius)

-- SingleShift:
-- series -> jumps map -> jump index -> jump height -> Maybe newHeight

zeroJump' :: SingleShift
zeroJump' _ _ _ _ = Just 0

estimateSlopeLeft' :: Int -> SingleShift
estimateSlopeLeft' radius ds jumps i _ =
  let boundedRadius = i - max 0 (i - radius)
      i0 = i-boundedRadius
      prevDiffs = V.map snd
        $ V.filter (not . flip M.member jumps . fst)
        $ V.zip (V.generate boundedRadius (+i0))
        $ V.slice i0 boundedRadius ds
  in  if V.null prevDiffs then Nothing else Just $ unboxedAverage prevDiffs

estimateSlopeRight' :: Int -> SingleShift
estimateSlopeRight' radius ds jumps i _ =
  let boundedRadius = min (V.length ds - 1) (i + radius) - i
      i0 = i+1
      nextDiffs = V.map snd
        $ V.filter (not . flip M.member jumps . fst)
        $ V.zip (V.generate boundedRadius (+i0))
        $ V.slice (i+1) boundedRadius ds
  in  if V.null nextDiffs then Nothing else Just $ unboxedAverage nextDiffs

-- Average the left and right estimates; or, if one side doesn't exist, use the
-- side that does.
estimateSlopeBoth' :: Int -> SingleShift
estimateSlopeBoth' radius ds jumps i x =
  let left  = estimateSlopeLeft'  radius ds jumps i x
      right = estimateSlopeRight' radius ds jumps i x
      avg   = fmap (/2) $ (+) <$> left <*> right
  in  avg <|> left <|> right

-------------------------------------------------------------------------------
-- Manual operations on pairs and groups of jumps
-------------------------------------------------------------------------------

-- 'Line'
interpolateBetweenJumps ::
     Double
  -> [Int]
  -> M.IntMap Double
  -> TraceStateOperator
interpolateBetweenJumps _ indices _ =
  unsafeTraceStateOperator $ \traceState ->
    case indices of
      (j0:_:_) ->
        let j1 = last indices
            v = traceState ^. series
            pointInterval = I.undiff $ I.fromEndpoints (j0, j1)
            yPair = over both (v V.!) $ I.getEndpoints pointInterval
            updates = I.interpolationUpdates pointInterval yPair
        in  updateDiffSeries 0 updates traceState
      _  -> traceState

-- 'Sum'
matchGroup :: Double -> [Int] -> M.IntMap Double -> TraceStateOperator
matchGroup offset indices jumps
  | (_:_:_) <- indices
  , Just slopes <- traverse (`M.lookup` jumps) indices
  = unsafeTraceStateOperator $ \traceState ->
      let ds = snd $ traceState ^. diffSeries
          slopeEsts = map (estimateSlope ds jumps radius) indices
            where radius = 4
          slopeErrors = zipWith (-) slopes slopeEsts
          err = sum slopeErrors + head slopeEsts
          newSlopes = err : tail slopeEsts
            & _last %~ subtract offset
            & _head %~ (+offset)
          updates = zip indices newSlopes
      in  updateDiffSeries 0 updates traceState
  | otherwise = idOperator
