-- Typesafe `Int`s for indexing into vectors.

-- This module exists because most errors during the this program's development
-- were caused by the mixup of `Int`s that were intended to index into either a
-- data vector `v` or its "derivative" `zipWith (-) (tail v) v`.

-- We define newtypes for `Int`s, and then tediously wrap the API of
-- Data.Vector, Data.IntSet, and Data.IntMap. For performance reasons, we have
-- currently adopted the practice of inlining _all_ wrappers, since we do not
-- know how to distinguish on a case-by-case basis whether inlining is
-- appropriate.

-- We also introduce an `IndexInterval` type, representing a contiguous interval
-- of `Int`s, since another source of error was/is ambiguity as to whether the
-- right endpoint is inclusive or exclusive.

-- Furthermore, sometimes functions in this module are specialized to work on
-- only a single type of Index. This is because I wanted to use `coerce` for
-- maximal efficiency, but didn't know how to get it to work polymorphically.

-- Also, many functions are commented out because they are were once used but
-- are currently unneeded. My reasoning is "might as well keep them around as
-- comments, for now".

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

  , iMinus
  , iTranslate

  , IndexInterval (..)
  , _IndexInterval

  , iiLeft
  , iiMember
  , iiShrink
  , iiGrow
  , iiLength
  , iiToVector1
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
  , iisFindNearestIndex
  , iisFindIntermediateIndices1

  , iisFromList1
  , iisToList1
  , iisSize
  , iisFilter
  , iisSplit
  , iisBound
  , iisMember

  , IIntMap

  -- , iimFindNearestIndex
  -- , iimFindIntermediateIndices1

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
-- `Index`s are newtypes of `Int` that may only be used to index into vectors
-- (`IVector`s) with a matching index type.

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

-- I intend for Index0, Index1, Index2 to be "affine with respect to Int"

{-# INLINE iMinus #-}
iMinus :: IsInt i => i -> i -> Int
iMinus i j = toInt i - toInt j

{-# INLINE iTranslate #-}
iTranslate :: IsInt i => Int -> i -> i
iTranslate i idx = fromInt $ i + toInt idx

--------------------------------------------------------------------------------
-- IndexInterval
--------------------------------------------------------------------------------
-- `IndexInterval`s are pairs of `Index`s, and also may only be used to index
-- into `IVector`s with a matching index type.

-- The purpose of this type is to avoid confusion as to whether the right
-- endpoint of an interval is inclusive or exclusive. `IndexInterval`s represent
-- _closed_ intervals in `Int`s.

-- We assume that l <= u in `IndexInterval (l, u)`, but this is not enforced.

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
iiLength (IndexInterval (l, u)) = u `iMinus` l + 1

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
iiDiff :: (IsInt i, IsInt (ISucc i))
       => IndexInterval i -> IndexInterval (ISucc i)
iiDiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                              , fromInt $ pred $ toInt u )

{-# INLINE iiUndiff #-}
iiUndiff :: (IsInt i, IsInt (ISucc i), Enum (ISucc i))
         => IndexInterval (ISucc i) -> IndexInterval i
iiUndiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                                , fromInt $ succ $ toInt u )

{-# INLINE levelShiftEndpoints #-}
levelShiftEndpoints :: Index1 -> IndexInterval Index0
levelShiftEndpoints j = iiUndiff $ IndexInterval (j, j)

--------------------------------------------------------------------------------
-- Indexed Vectors
--------------------------------------------------------------------------------
-- An "indexed vector" `IVector i a` is a newtype for `Vector a` that may only
-- be indexed by elements of its phantom type. This precludes many functions
-- from the regular Vector API that do not preserve indices, like folding,
-- filtering, and slicing.

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

-- We use (i ~ Void) in `IVector i a` to disallow interaction with the indexing
-- type so that the only possible fate of the IVector is to be consumed by one
-- of the "reducing" functions in this module, such as `ivCount`, or simply
-- extracted by `unsafeRunIVector`.
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

-- "Approved" folds

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

-- Functions requiring special-case index conversions

{-# INLINE interpolationUpdates #-}
interpolationUpdates
  :: IVector Index0 Double -> IndexInterval Index0 -> [(Index1, Double)]
interpolationUpdates v interval@(IndexInterval (l, u)) =
  let xSpan = toInt u - toInt l
      ySpan = ivIndex v u - ivIndex v l
      avgSlope = ySpan / fromIntegral xSpan
  in  zip (uncurry enumFromTo $ runIndexInterval $ iiDiff interval)
          (repeat avgSlope)

--------------------------------------------------------------------------------
-- Data.Vector.Unboxed API wrappers
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

{-# INLINE ivFindIndices2 #-}
ivFindIndices2 :: (V.Unbox a)
              => (a -> Bool) -> IVector Index2 a -> V.Vector Index2
ivFindIndices2 f (IVector v) = coerce $ V.findIndices f v

--------------------------------------------------------------------------------
-- Indexed IntSets
--------------------------------------------------------------------------------
-- An "indexed IntSet" `IIntSet i` is newtype for `IntSet` restricted to work
-- only with the elements of its phantom type.

newtype IIntSet i   = IIntSet { runIIntSet :: S.IntSet }
  deriving (Monoid)

{-# INLINE iisTranslate #-}
iisTranslate :: IsInt i => Int -> IIntSet i -> IIntSet i
iisTranslate i (IIntSet s) = IIntSet $ S.fromAscList $ map (+i) $ S.toAscList s

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
-- Data.IntSet API wrappers
--------------------------------------------------------------------------------

{-# INLINE iisFromList1 #-}
iisFromList1 :: [Index1] -> IIntSet Index1
iisFromList1 = IIntSet . S.fromList . coerce

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

--------------------------------------------------------------------------------
-- Indexed `IntMap`s
--------------------------------------------------------------------------------
-- An "indexed IntMap" `IIntMap i a` is newtype for `IntMap a` restricted to
-- work only with keys of its phantom type.

newtype IIntMap i a = IIntMap { runIIntMap :: M.IntMap a }

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

--------------------------------------------------------------------------------
-- Data.IntMap.Strict API wrappers
--------------------------------------------------------------------------------

-- {-# INLINE iimFromList1 #-}
-- iimFromList1 :: [(Index1, a)] -> IIntMap Index1 a
-- iimFromList1 = IIntMap . M.fromList . coerce

{-# INLINE iimFromSet #-}
iimFromSet :: IsInt i => (i -> a) -> IIntSet i -> IIntMap i a
iimFromSet f (IIntSet s) = IIntMap $ M.fromSet (f . fromInt) s

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
