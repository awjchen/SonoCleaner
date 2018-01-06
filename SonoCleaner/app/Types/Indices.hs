-- Typesafe `Int`s for indexing into vectors.

-- This module exists because most errors during the this program's development
-- were caused by the mixup of `Int`s that could index into either a data vector
-- `v` or its "derivative" `zipWith (-) (tail v) v`.

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.Indices
  ( Index0, index0, unsafeRunIndex0
  , Index1, index1
  , Index2

  , minus
  , translate

  , IndexInterval (..)
  , _IndexInterval

  , iiLeft
  , iiMember
  , iiShrink
  , iiGrow
  , iiLength
  -- , iiToList
  , iiToVector1
  -- , iiToIVector
  , iiIndex
  , iiBound
  , iiGetIVectorBounds
  , iiBoundByIVector
  , iiIsSingleton
  , iiSlice

  , iiDiff
  , iiUndiff
  , levelShiftEndpoints

  , IVector
  , ivector
  , unsafeRunIVector

  , ivDiff
  , ivUndiff

  , ivIndex
  , ivSlice
  , unsafeIvSlice

  -- , ivExtend0
  , ivExtend1
  , ivExtend2

  -- , ivAverage
  , minMax
  , ivMinMax
  , ivCount
  , interpolationUpdates

  , (*//)
  , ivUpdate
  , ivIndexed1
  , ivMap
  , ivZip
  , ivUnzip
  , ivFindIndices2

  , IIntSet

  , iisTranslate

  , iisFromList1
  , iisToList1
  , iisSize
  , iisFilter
  , iisSplit
  , iisBound
  , iisMember
  , iisFindNearestIndex
  , iisFindIntermediateIndices1

  , IIntMap

  -- , iimFromList1
  , iimFromSet
  , iimToList1
  -- , iimSize
  -- , iimKeys1
  -- , iimMap
  , iimMapWithKey
  -- , iimMember
  , iimLookup
  -- , iimUnionWith
  -- , iimSplit
  -- , iimBound
  -- , iimFindNearestIndex
  -- , iimFindIntermediateIndices1
  ) where

import           Control.Applicative
import           Control.Arrow                ((***))
import           Control.Lens
import           Control.Monad
import           Data.Coerce
import qualified Data.IntMap                  as M
import qualified Data.IntSet                  as S
import qualified Data.Vector.Unboxed          as V
import           Data.Vector.Unboxed.Deriving
import           Data.Void

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

-- `Index`s are newtypes of `Int` that may only be used to index into `IVector`s
-- with a matching index type.

newtype Index0 = Index0 { runIndex0 :: Int }
  deriving (Eq, Ord, Enum)

newtype Index1 = Index1 { runIndex1 :: Int }
  deriving (Eq, Ord, Enum)

newtype Index2 = Index2 { runIndex2 :: Int }
  deriving (Eq, Ord, Enum)

{-# INLINE index0 #-}
index0 :: Int -> Index0
index0 = coerce

{-# INLINE unsafeRunIndex0 #-}
unsafeRunIndex0 :: Index0 -> Int
unsafeRunIndex0 = coerce

{-# INLINE index1 #-}
index1 :: Int -> Index1
index1 = coerce

{-# INLINE index2 #-}
index2 :: Int -> Index2
index2 = coerce

class IsInt i where
  toInt :: i -> Int
  fromInt :: Int -> i

instance IsInt Index0 where
  {-# INLINE toInt #-}
  toInt = coerce
  {-# INLINE fromInt #-}
  fromInt = coerce

instance IsInt Index1 where
  {-# INLINE toInt #-}
  toInt = coerce
  {-# INLINE fromInt #-}
  fromInt = coerce

instance IsInt Index2 where
  {-# INLINE toInt #-}
  toInt = coerce
  {-# INLINE fromInt #-}
  fromInt = coerce

derivingUnbox "Index0" [t| Index0 -> Int |] [| coerce |] [| coerce |]
derivingUnbox "Index1" [t| Index1 -> Int |] [| coerce |] [| coerce |]
derivingUnbox "Index2" [t| Index2 -> Int |] [| coerce |] [| coerce |]

type family ISucc i where
  ISucc Index0 = Index1
  ISucc Index1 = Index2

{-# INLINE minus #-}
minus :: IsInt i => i -> i -> Int
minus i j = toInt i - toInt j

{-# INLINE translate #-}
translate :: IsInt i => Int -> i -> i
translate i idx = fromInt $ i + toInt idx

--------------------------------------------------------------------------------
-- IndexInterval
--------------------------------------------------------------------------------

-- `IndexInterval`s are pairs of `Index`s, and also may only be used to index
-- into `IVector`s with a matching index type.

-- The purpose of this type is to avoid confusion as to whether the right
-- endpoint of an interval is inclusive or exclusive. `IndexInterval`s represent
-- _closed_ intervals in `Int`s.

-- We assume that l <= u in `IndexInterval (l, u)`.

newtype IndexInterval i = IndexInterval { runIndexInterval :: (i, i) }
  deriving (Eq)
makePrisms ''IndexInterval

derivingUnbox "IndexInterval0"
  [t| IndexInterval Index0 -> (Int, Int) |] [| coerce |] [| coerce |]
derivingUnbox "IndexInterval1"
  [t| IndexInterval Index1 -> (Int, Int) |] [| coerce |] [| coerce |]
derivingUnbox "IndexInterval2"
  [t| IndexInterval Index2 -> (Int, Int) |] [| coerce |] [| coerce |]

-- instance Ord i => Semigroup (IndexInterval i) where
--   (IndexInterval (l, u)) <> (IndexInterval (l', u')) =
--     IndexInterval (min l l', max u u')

{-# INLINE iiLeft #-}
iiLeft :: IndexInterval i -> i
iiLeft (IndexInterval (l, _)) = l

{-# INLINE iiMember #-}
iiMember :: Ord i => i -> IndexInterval i -> Bool
iiMember i (IndexInterval (l, u)) = l <= i && i <= u

{-# INLINE iiShrink #-}
iiShrink :: Enum i => IndexInterval i -> IndexInterval i
iiShrink (IndexInterval (l, u)) = IndexInterval (succ l, pred u)

{-# INLINE iiGrow #-}
iiGrow :: Enum i => IndexInterval i -> IndexInterval i
iiGrow (IndexInterval (l, u)) = IndexInterval (pred l, succ u)

{-# INLINE iiLength #-}
iiLength :: IsInt i => IndexInterval i -> Int
iiLength (IndexInterval (l, u)) = u `minus` l + 1

{-# INLINE iiToList #-}
iiToList :: Enum i => IndexInterval i -> [i]
iiToList (IndexInterval (l, u)) = [l..u]

{-# INLINE iiToVector1 #-}
iiToVector1 :: IndexInterval Index1 -> V.Vector Index1
iiToVector1 (IndexInterval (l, u)) = coerce $ V.enumFromN l' (u'-l'+1)
  where l' = toInt l; u' = toInt u

{-# INLINE iiIndex #-}
iiIndex :: (IsInt i, V.Unbox a) => IVector i a -> IndexInterval i -> (a, a)
iiIndex (IVector v) (IndexInterval (l, u)) = (v V.! toInt l, v V.! toInt u)

{-# INLINE iiBound #-}
iiBound :: Ord i => IndexInterval i -> i -> i
iiBound (IndexInterval (l, u)) i
  | i < l = l
  | i > u = u
  | otherwise = i

{-# INLINE iiGetIVectorBounds #-}
iiGetIVectorBounds
  :: (IsInt i, V.Unbox a) => IVector i a -> IndexInterval i
iiGetIVectorBounds (IVector v) =
  IndexInterval (fromInt 0, fromInt $ V.length v - 1)

{-# INLINE iiBoundByIVector #-}
iiBoundByIVector :: (IsInt i, V.Unbox a)
        => IVector i a -> IndexInterval i -> IndexInterval i
iiBoundByIVector (IVector v) (IndexInterval (l, u)) =
  IndexInterval ( fromInt $ max 0 $ toInt l
                , fromInt $ min (V.length v - 1) $ toInt u)

{-# INLINE iiIsSingleton #-}
iiIsSingleton :: Eq i => IndexInterval i -> Bool
iiIsSingleton (IndexInterval (l, u)) = l == u

{-# INLINE iiSlice #-}
iiSlice :: V.Unbox a => IndexInterval Int -> V.Vector a -> V.Vector a
iiSlice (IndexInterval (l, u)) = V.slice l (u-l+1)

-- Index conversions

{-# INLINE iiDiff #-}
iiDiff :: (IsInt i, IsInt (ISucc i), Enum i)
       => IndexInterval i -> IndexInterval (ISucc i)
iiDiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                              , fromInt $ toInt $ pred u )

{-# INLINE iiUndiff #-}
iiUndiff :: (IsInt i, IsInt (ISucc i), Enum (ISucc i))
         => IndexInterval (ISucc i) -> IndexInterval i
iiUndiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                                , fromInt $ toInt $ succ u )

{-# INLINE levelShiftEndpoints #-}
levelShiftEndpoints :: Index1 -> IndexInterval Index0
levelShiftEndpoints j = iiUndiff $ IndexInterval (j, j)

--------------------------------------------------------------------------------
-- Indexed vectors
--------------------------------------------------------------------------------

-- The individual elements of an `IVector i a` may only be accessed through the
-- use of indices of type `i`. This precludes many functions from the regular
-- Vector API, like folding, but also those which do not preserve indices, like
-- filtering or slicing.
newtype IVector i a = IVector { runIVector :: V.Vector a }

{-# INLINE ivector #-}
ivector :: V.Vector a -> IVector i a
ivector = IVector

{-# INLINE unsafeRunIVector #-}
unsafeRunIVector :: IVector i a -> V.Vector a
unsafeRunIVector = runIVector

-- Index conversions

{-# INLINE ivDiff #-}
ivDiff :: (Num a, V.Unbox a) => IVector i a -> (a, IVector (ISucc i) a)
ivDiff (IVector v) = (V.head v, IVector $ V.zipWith (-) (V.tail v) v)

{-# INLINE ivUndiff #-}
ivUndiff :: (Num a, V.Unbox a) => (a, IVector (ISucc i) a) -> IVector i a
ivUndiff (h, (IVector v)) = IVector $ V.scanl' (+) h v

-- Indexing by typesafe indices

{-# INLINE ivIndex #-}
ivIndex :: (IsInt i, V.Unbox a) => IVector i a -> i -> a
ivIndex (IVector v) i = v V.! toInt i

-- Using Void disallows interaction with the indexing type so that the only
-- possible fate of the IVector is to be consumed by one of the "reducing"
-- functions in this module, such as `ivCount`, or simply extracted by
-- `unsafeRunIVector`.
{-# INLINE ivSlice #-}
ivSlice :: (IsInt i, V.Unbox a)
        => IndexInterval i -> IVector i a -> IVector Void a
ivSlice ii iv = coerce $ unsafeIvSlice ii iv

{-# INLINE unsafeIvSlice #-}
unsafeIvSlice :: (IsInt i, V.Unbox a)
              => IndexInterval i -> IVector i a -> IVector i a
unsafeIvSlice (IndexInterval (i, j)) (IVector v) =
  let i' = toInt i; j' = toInt j
  in  IVector $ V.slice i' (j'-i'+1) v

-- Index-specific functions

-- {-# INLINE ivExtend0 #-}
-- ivExtend0 :: V.Unbox a => Int -> IVector Index0 a -> IVector Index0 a
-- ivExtend0 r (IVector v) = IVector $ V.concat
--   [ V.replicate r (V.head v)
--   , v
--   , V.replicate r (V.last v) ]

{-# INLINE ivExtend1 #-}
ivExtend1 :: (V.Unbox a, Num a) => Int -> IVector Index1 a -> IVector Index1 a
ivExtend1 r (IVector dv) = IVector $ V.concat
  [ V.replicate r 0
  , dv
  , V.replicate r 0 ]

{-# INLINE ivExtend2 #-}
ivExtend2 :: (V.Unbox a, Num a) => Int -> IVector Index2 a -> IVector Index2 a
ivExtend2 r (IVector ddv) = IVector $ V.concat
  [ V.replicate (r-1) 0
  , V.singleton (V.head ddv)
  , ddv
  , V.singleton (negate $ V.last ddv)
  , V.replicate (r-1) 0 ]

-- Special functions

-- {-# INLINE ivAverage #-}
-- ivAverage :: IVector i Double -> Double
-- ivAverage (IVector v) = V.sum v / fromIntegral (V.length v)

{-# INLINE ivMinMax #-}
ivMinMax :: (V.Unbox a, Ord a) => IVector i a -> (a, a)
ivMinMax (IVector v) = minMax v

{-# INLINE minMax #-}
minMax :: (V.Unbox a, Ord a) => V.Vector a -> (a, a)
minMax v = let z = V.head v in V.foldl' minMaxAcc (z, z) (V.tail v) where
  minMaxAcc :: Ord a => (a, a) -> a -> (a, a)
  minMaxAcc (minAcc, maxAcc) x =
    let minAcc' = min x minAcc
        maxAcc' = max x maxAcc
    in  minAcc' `seq` maxAcc' `seq` (minAcc', maxAcc')

{-# INLINE ivCount #-}
ivCount :: V.Unbox a => (a -> Bool) -> IVector i a -> Int
ivCount f (IVector v) = V.length $ V.filter f v

{-# INLINE interpolationUpdates #-}
interpolationUpdates
  :: IVector Index0 Double -> IndexInterval Index0 -> [(Index1, Double)]
interpolationUpdates v interval@(IndexInterval (l, u)) =
  let xSpan = toInt u - toInt l
      ySpan = ivIndex v u - ivIndex v l
      avgSlope = ySpan / fromIntegral xSpan
  in  zip (iiToList $ iiDiff interval) (repeat avgSlope)

--------------------------------------------------------------------------------
-- Indexed `Vectors` -- Vector API
--------------------------------------------------------------------------------

-- Modifying vectors

-- * is just an arbitrary prefix, since 'iv' canont be used.
{-# INLINE (*//) #-}
(*//) :: V.Unbox a => IVector Index1 a -> [(Index1, a)] -> IVector Index1 a
(*//) (IVector v) updates = IVector $ v V.// (coerce updates)

{-# INLINE ivUpdate #-}
ivUpdate :: V.Unbox a
         => IVector Index1 a -> IVector Index1 (Index1, a) -> IVector Index1 a
ivUpdate (IVector v) (IVector updates) =
  IVector $ V.update v (V.map coerce updates)

-- Elementwise operations

{-# INLINE _ivIndices1 #-}
_ivIndices1 :: V.Unbox a => IVector Index1 a -> IVector Index1 Index1
_ivIndices1 (IVector v) = IVector $ coerce $ V.enumFromN 0 (V.length v)

{-# INLINE ivIndexed1 #-}
ivIndexed1 :: V.Unbox a => IVector Index1 a -> IVector Index1 (Index1, a)
ivIndexed1 iv = ivZip (_ivIndices1 iv) iv

{-# INLINE ivMap #-}
ivMap :: (V.Unbox a, V.Unbox b) => (a -> b) -> IVector i a -> IVector i b
ivMap f (IVector v) = IVector $ V.map f v

{-# INLINE ivZip #-}
ivZip :: (V.Unbox a, V.Unbox b)
      => IVector i a -> IVector i b -> IVector i (a, b)
ivZip (IVector v) (IVector w) = IVector $ V.zip v w

{-# INLINE ivUnzip #-}
ivUnzip :: (V.Unbox a, V.Unbox b)
      => IVector i (a, b) -> (IVector i a, IVector i b)
ivUnzip (IVector v) = IVector *** IVector $ V.unzip v

-- The type is specialized because I don't know how use coerce polymorphically.
{-# INLINE ivFindIndices2 #-}
ivFindIndices2 :: (V.Unbox a)
              => (a -> Bool) -> IVector Index2 a -> V.Vector Index2
ivFindIndices2 f (IVector v) = coerce $ V.findIndices f v

--------------------------------------------------------------------------------
-- Indexed `IntSet`s
--------------------------------------------------------------------------------

newtype IIntSet i   = IIntSet { runIIntSet :: S.IntSet }
  deriving (Monoid)

{-# INLINE iisTranslate #-}
iisTranslate :: IsInt i => Int -> IIntSet i -> IIntSet i
iisTranslate i (IIntSet s) = IIntSet $ S.fromAscList $ map (+i) $ S.toAscList s

-- The type is specialized because I don't know how use coerce polymorphically.
{-# INLINE iisFromList1 #-}
iisFromList1 :: [Index1] -> IIntSet Index1
iisFromList1 = IIntSet . S.fromList . coerce

-- The type is specialized because I don't know how use coerce polymorphically.
{-# INLINE iisToList1 #-}
iisToList1 :: IIntSet a -> [Index1]
iisToList1 (IIntSet s) = coerce $ S.toList s

{-# INLINE iisSize #-}
iisSize :: IIntSet a -> Int
iisSize (IIntSet s) = S.size s

{-# INLINE iisFilter #-}
iisFilter :: IsInt i => (i -> Bool) -> IIntSet i -> IIntSet i
iisFilter f (IIntSet s) = IIntSet $ S.filter (f . fromInt) s

{-# INLINE iisSplit #-}
iisSplit :: IsInt i => i -> IIntSet i -> (IIntSet i, IIntSet i)
iisSplit i (IIntSet s) = IIntSet *** IIntSet $ S.split (toInt i) s

{-# INLINE iisBound #-}
iisBound :: IsInt i => IndexInterval i -> IIntSet i -> IIntSet i
iisBound (IndexInterval (l, u)) (IIntSet s) =
  IIntSet $ fst $ S.split (succ $ toInt u) $ snd $ S.split (pred $ toInt l) s

{-# INLINE iisMember #-}
iisMember :: IsInt i => i -> IIntSet i -> Bool
iisMember i (IIntSet s) = S.member (toInt i) s

{-# INLINE iisFindNearestIndex #-}
iisFindNearestIndex :: IsInt i => i -> IIntSet i -> Maybe i
iisFindNearestIndex target (IIntSet s) =
  fmap fromInt $ nearest <|> lower <|> upper
  where
    target' = toInt target
    lower = S.lookupLE target' s
    upper = S.lookupGE target' s
    nearest = do
      l <- lower
      u <- upper
      if abs (l - target') <= abs (u - target')
        then pure l else pure u

-- The type is specialized because I don't know how use coerce polymorphically.
{-# INLINE iisFindIntermediateIndices1 #-}
iisFindIntermediateIndices1
  :: IndexInterval Index1 -> IIntSet Index1 -> Maybe [Index1]
iisFindIntermediateIndices1 (IndexInterval (low, high)) s@(IIntSet s') = do
  let low' = toInt low; high' = toInt high
  lowIndex  <- S.lookupGE low'  s'
  highIndex <- S.lookupLE high' s'
  guard (lowIndex <= highIndex)
  let interval = IndexInterval (fromInt lowIndex, fromInt highIndex)
  pure $ iisToList1 $ iisBound interval s

--------------------------------------------------------------------------------
-- Indexed `IntMap`s
--------------------------------------------------------------------------------

newtype IIntMap i a = IIntMap { runIIntMap :: M.IntMap a }

-- The type is specialized because I don't know how use coerce polymorphically.
-- iimFromList1 :: [(Index1, a)] -> IIntMap Index1 a
-- iimFromList1 = IIntMap . M.fromList . coerce

{-# INLINE iimFromSet #-}
iimFromSet :: IsInt i => (i -> a) -> IIntSet i -> IIntMap i a
iimFromSet f (IIntSet s) = IIntMap $ M.fromSet (f . fromInt) s

-- The type is specialized because I don't know how use coerce polymorphically.
{-# INLINE iimToList1 #-}
iimToList1 :: IIntMap Index1 a -> [(Index1, a)]
iimToList1 (IIntMap m) = coerce $ M.toList m

-- {-# INLINE iimSize #-}
-- iimSize :: IIntMap i a -> Int
-- iimSize (IIntMap m) = M.size m

-- {-# INLINE iimKeys1 #-}
-- iimKeys1 :: IIntMap Index1 a -> [Index1]
-- iimKeys1 (IIntMap m) = coerce $ M.keys m

-- {-# INLINE iimMap #-}
-- iimMap :: (a -> b) -> IIntMap i a -> IIntMap i b
-- iimMap f (IIntMap m) = IIntMap $ M.map f m

{-# INLINE iimMapWithKey #-}
iimMapWithKey :: IsInt i => (i -> a -> b) -> IIntMap i a -> IIntMap i b
iimMapWithKey f (IIntMap m) = IIntMap $ M.mapWithKey (f . fromInt) m

-- {-# INLINE iimMember #-}
-- iimMember :: IsInt i => i -> IIntMap i a -> Bool
-- iimMember i (IIntMap m) = M.member (toInt i) m

{-# INLINE iimLookup #-}
iimLookup :: IsInt i => i -> IIntMap i a -> Maybe a
iimLookup i (IIntMap m) = M.lookup (toInt i) m

-- {-# INLINE iimUnionWith #-}
-- iimUnionWith :: (a -> a -> a) -> IIntMap i a -> IIntMap i a -> IIntMap i a
-- iimUnionWith f (IIntMap m1) (IIntMap m2) = IIntMap $ M.unionWith f m1 m2

-- {-# INLINE iimSplit #-}
-- iimSplit :: IsInt i => i -> IIntMap i a -> (IIntMap i a, IIntMap i a)
-- iimSplit i (IIntMap m) = IIntMap *** IIntMap $ M.split (toInt i) m

-- {-# INLINE iimBound #-}
-- iimBound :: IsInt i => IndexInterval i -> IIntMap i a -> IIntMap i a
-- iimBound (IndexInterval (l, u)) (IIntMap m) =
--   IIntMap $ fst $ M.split (succ $ toInt u) $ snd $ M.split (pred $ toInt l) m

-- {-# INLINE iimFindNearestIndex #-}
-- iimFindNearestIndex :: IsInt i => i -> IIntMap i a -> Maybe i
-- iimFindNearestIndex target (IIntMap m) =
--   fmap fromInt $ nearest <|> lower <|> upper
--   where
--     target' = toInt target
--     lower = fst <$> M.lookupLE target' m
--     upper = fst <$> M.lookupGE target' m
--     nearest = do
--       l <- lower
--       u <- upper
--       if abs (l - target') <= abs (u - target')
--         then pure l else pure u

-- -- The type is specialized because I don't know how use coerce polymorphically.
-- {-# INLINE iimFindIntermediateIndices1 #-}
-- iimFindIntermediateIndices1
--   :: IndexInterval Index1 -> IIntMap Index1 a -> Maybe [Index1]
-- iimFindIntermediateIndices1 (IndexInterval (low, high)) m@(IIntMap m') = do
--   let low' = toInt low; high' = toInt high
--   lowIndex  <- fst <$> M.lookupGE low'  m'
--   highIndex <- fst <$> M.lookupLE high' m'
--   guard (lowIndex <= highIndex)
--   let interval = IndexInterval (fromInt lowIndex, fromInt highIndex)
--   pure $ map fst $ iimToList1 $ iimBound interval m
