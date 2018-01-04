-- Typesafe `Int`s for indexing into vectors.

-- This module exists because most errors during the this program's development
-- were caused by the mixup of `Int`s indexing into a data vector `v` or its
-- "derivative" `zipWith (-) (tail v) v`.

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types.Indices
  ( Index0, index0, unsafeRunIndex0
  , Index1, index1
  , Index2, index2

  , IndexInterval (..)
  , _IndexInterval

  , iiLeft
  , iiMember
  , iiShrink
  , iiGrow
  , iiToList
  , iiToVector
  , iiToIVector
  , iiIndex
  , iiBoundToIVector

  , iiDiff
  , iiUndiff

  , IVector
  , ivector
  , unsafeRunIVector 

  , ivDiff
  , ivUndiff

  , ivIndex
  , ivSlice

  , ivExtend0
  , ivExtend1
  , ivExtend2

  , ivAverage
  , ivMinMax
  , ivCount
  , interpolationUpdates

  , ivLength
  , ivEnumFromN
  , (*//)
  , ivUpdate
  , ivIndexed
  , ivMap
  , ivZip
  , ivFindIndices2

  , IIntSet

  , iisOffset

  , iisFromList1
  , iisFilter

  , IIntMap

  , iimFromList1
  , iimToList1
  , iimMapWithKey
  , iimMember
  , iimLookup
  , iimUnionWith
  ) where

import           Control.Lens
import           Data.Coerce
import qualified Data.IntMap                  as M
import qualified Data.IntSet                  as S
import qualified Data.Vector.Unboxed          as V
import           Data.Vector.Unboxed.Deriving

--------------------------------------------------------------------------------
-- Index
--------------------------------------------------------------------------------

-- `Index`s are newtypes of `Int` that may only be used to index into `IVector`s
-- with a matching index type.

class IsInt i where
  toInt :: i -> Int
  fromInt :: Int -> i

newtype Index0 = Index0 { runIndex0 :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype Index1 = Index1 { runIndex1 :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

newtype Index2 = Index2 { runIndex2 :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

index0 :: Int -> Index0
index0 = coerce

unsafeRunIndex0 :: Index0 -> Int
unsafeRunIndex0 = coerce

index1 :: Int -> Index1
index1 = coerce

index2 :: Int -> Index2
index2 = coerce

instance IsInt Index0 where
  toInt = coerce
  fromInt = coerce

instance IsInt Index1 where
  toInt = coerce
  fromInt = coerce

instance IsInt Index2 where
  toInt = coerce
  fromInt = coerce

derivingUnbox "Index0" [t| Index0 -> Int |] [| coerce |] [| coerce |]
derivingUnbox "Index1" [t| Index1 -> Int |] [| coerce |] [| coerce |]
derivingUnbox "Index2" [t| Index2 -> Int |] [| coerce |] [| coerce |]

type family ISucc i where
  ISucc Index0 = Index1
  ISucc Index1 = Index2

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
makePrisms ''IndexInterval

derivingUnbox "IndexInterval0"
  [t| IndexInterval Index0 -> (Int, Int) |] [| coerce |] [| coerce |]
derivingUnbox "IndexInterval1"
  [t| IndexInterval Index1 -> (Int, Int) |] [| coerce |] [| coerce |]
derivingUnbox "IndexInterval2"
  [t| IndexInterval Index2 -> (Int, Int) |] [| coerce |] [| coerce |]

iiLeft :: IndexInterval i -> i
iiLeft (IndexInterval (l, _)) = l

iiMember :: Ord i => i -> IndexInterval i -> Bool
iiMember i (IndexInterval (l, u)) = l <= i && i <= u

iiShrink :: Enum i => IndexInterval i -> IndexInterval i
iiShrink (IndexInterval (l, u)) = IndexInterval (succ l, pred u)

iiGrow :: Enum i => IndexInterval i -> IndexInterval i
iiGrow (IndexInterval (l, u)) = IndexInterval (pred l, succ u)

iiToList :: Enum i => IndexInterval i -> [i]
iiToList (IndexInterval (l, u)) = [l..u]

iiToVector :: (IsInt i, V.Unbox i, Num i) => IndexInterval i -> V.Vector i
iiToVector (IndexInterval (l, u)) = V.enumFromN l (toInt $ u-l+1)

iiToIVector :: (IsInt i, V.Unbox i, Num i) => IndexInterval i -> IVector i i
iiToIVector (IndexInterval (l, u)) = IVector $ V.enumFromN l (toInt $ u-l+1)

iiIndex :: (IsInt i, V.Unbox a) => IVector i a -> IndexInterval i -> (a, a)
iiIndex (IVector v) (IndexInterval (l, u)) = (v V.! toInt l, v V.! toInt u)

iiBoundToIVector :: (IsInt i, V.Unbox a)
        => IVector i a -> IndexInterval i -> IndexInterval i
iiBoundToIVector (IVector v) (IndexInterval (l, u)) =
  IndexInterval ( fromInt $ max 0 $ toInt l
                , fromInt $ min (V.length v - 1) $ toInt u)

-- Index conversions

iiDiff :: (IsInt i, IsInt (ISucc i), Enum i)
       => IndexInterval i -> IndexInterval (ISucc i)
iiDiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                              , fromInt $ toInt $ pred u )

iiUndiff :: (IsInt i, IsInt (ISucc i), Enum (ISucc i))
         => IndexInterval (ISucc i) -> IndexInterval i
iiUndiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                                , fromInt $ toInt $ succ u )

--------------------------------------------------------------------------------
-- Indexed vectors
--------------------------------------------------------------------------------

-- The individual elements of an `IVector i a` may only be accessed through the
-- use of indices of type `i`. This precludes many functions from the regular
-- Vector API, like folding, but also those which do not preserve indices, like
-- filtering. However, our notion of 'index-preserving' allows for "translation"
-- of the indices so that we can do slicing.
newtype IVector i a = IVector { runIVector :: V.Vector a }
  deriving (Monoid)

ivector :: V.Vector a -> IVector i a
ivector = IVector

unsafeRunIVector :: IVector i a -> V.Vector a
unsafeRunIVector = runIVector

-- Index conversions

ivDiff :: (Num a, V.Unbox a) => IVector i a -> (a, IVector (ISucc i) a)
ivDiff (IVector v) = (V.head v, IVector $ V.zipWith (-) (V.tail v) v)

ivUndiff :: (Num a, V.Unbox a) => (a, IVector (ISucc i) a) -> IVector i a
ivUndiff (h, (IVector v)) = IVector $ V.scanl' (+) h v

-- Indexing by typesafe indices

ivIndex :: (IsInt i, V.Unbox a) => IVector i a -> i -> a
ivIndex (IVector v) i = v V.! toInt i

ivSlice :: (IsInt i, V.Unbox a) => IndexInterval i -> IVector i a -> IVector i a
ivSlice (IndexInterval (i, j)) (IVector v) =
  let i' = toInt i; j' = toInt j
  in  IVector $ V.slice i' (j'-i'+1) v

-- Index-specific functions

ivExtend0 :: V.Unbox a => Int -> IVector Index0 a -> IVector Index0 a
ivExtend0 r (IVector v) = IVector $ V.concat
  [ V.replicate r (V.head v)
  , v
  , V.replicate r (V.last v) ]

ivExtend1 :: (V.Unbox a, Num a) => Int -> IVector Index1 a -> IVector Index1 a
ivExtend1 r (IVector dv) = IVector $ V.concat
  [ V.replicate r 0
  , dv
  , V.replicate r 0 ]

ivExtend2 :: (V.Unbox a, Num a) => Int -> IVector Index2 a -> IVector Index2 a
ivExtend2 r (IVector ddv) = IVector $ V.concat
  [ V.replicate (r-1) 0
  , V.singleton (V.head ddv)
  , ddv
  , V.singleton (negate $ V.last ddv)
  , V.replicate (r-1) 0 ]

-- Special functions

ivAverage :: IVector i Double -> Double
ivAverage (IVector v) = V.sum v / fromIntegral (V.length v)

ivMinMax :: (V.Unbox a, Ord a) => IVector i a -> (a, a)
ivMinMax (IVector v) =
  let z = V.head v in V.foldl' minMaxAcc (z, z) (V.tail v) where

  minMaxAcc :: Ord a => (a, a) -> a -> (a, a)
  minMaxAcc (minAcc, maxAcc) x =
    let minAcc' = min x minAcc
        maxAcc' = max x maxAcc
    in  minAcc' `seq` maxAcc' `seq` (minAcc', maxAcc')

ivCount :: V.Unbox a => (a -> Bool) -> IVector i a -> Int
ivCount f (IVector v) = V.length $ V.filter f v

-- Does not use internals
interpolationUpdates
  :: IVector Index0 Double -> IndexInterval Index0 -> [(Index1, Double)]
interpolationUpdates v interval@(IndexInterval (l, u)) =
  let xSpan = u - l
      ySpan = ivIndex v u - ivIndex v l
      avgSlope = ySpan / fromIntegral xSpan
  in  zip (iiToList $ iiDiff interval) (repeat avgSlope)

--------------------------------------------------------------------------------
-- Indexed `Vectors` -- Vector API
--------------------------------------------------------------------------------

-- Accessors

ivLength :: (IsInt i, V.Unbox a) => IVector i a -> i
ivLength = fromInt . V.length . coerce

-- Construction

ivEnumFromN :: (IsInt i, Num a, V.Unbox a) =>  a -> i -> IVector i a
ivEnumFromN z i = IVector $ V.enumFromN z (toInt i)

-- ivConcat :: V.Unbox a => [IVector i a] -> IVector i a
-- ivConcat = IVector . V.concat . coerce

-- Modifying vectors

-- * is just an arbitrary prefix, since 'iv' canont be used.
(*//) :: V.Unbox a => IVector Index1 a -> [(Index1, a)] -> IVector Index1 a
(*//) (IVector v) updates = IVector $ v V.// (coerce updates)

ivUpdate :: V.Unbox a
         => IVector Index1 a -> IVector Index1 (Index1, a) -> IVector Index1 a
ivUpdate (IVector v) (IVector updates) =
  IVector $ V.update v (V.map coerce updates)

-- Elementwise operations

_ivIndices :: (V.Unbox i, Num i, V.Unbox a) => IVector i a -> IVector i i
_ivIndices (IVector v) = IVector $ V.enumFromN 0 (V.length v)

ivIndexed :: (V.Unbox i, Num i, V.Unbox a) => IVector i a -> IVector i (i, a)
ivIndexed iv = ivZip (_ivIndices iv) iv

ivMap :: (V.Unbox a, V.Unbox b) => (a -> b) -> IVector i a -> IVector i b
ivMap f (IVector v) = IVector $ V.map f v

ivZip :: (V.Unbox a, V.Unbox b)
      => IVector i a -> IVector i b -> IVector i (a, b)
ivZip (IVector v) (IVector w) = IVector $ V.zip v w

-- The type is specialized because I don't know how use coerce polymorphically.
ivFindIndices2 :: (V.Unbox a)
              => (a -> Bool) -> IVector Index2 a -> V.Vector Index2
ivFindIndices2 f (IVector v) = coerce $ V.findIndices f v

--------------------------------------------------------------------------------
-- Indexed `IntSet`s
--------------------------------------------------------------------------------

newtype IIntSet i   = IIntSet { runIIntSet :: S.IntSet }
  deriving (Monoid)

-- _iintset :: S.IntSet -> IIntSet i
-- _iintset = IIntSet

iisOffset :: IsInt i => i -> IIntSet i -> IIntSet i
iisOffset i (IIntSet s) =
  let i' = toInt i in IIntSet $ S.fromAscList $ map (+i') $ S.toAscList s

--------------------------------------------------------------------------------
-- Indexed `IntSet`s -- IntSet API
--------------------------------------------------------------------------------

-- The type is specialized because I don't know how use coerce polymorphically.
iisFromList1 :: [Index1] -> IIntSet Index1
iisFromList1 = IIntSet . S.fromList . coerce

iisFilter :: IsInt i => (i -> Bool) -> IIntSet i -> IIntSet i
iisFilter f (IIntSet s) = IIntSet $ S.filter (f . fromInt) s

--------------------------------------------------------------------------------
-- Indexed `IntMap`s
--------------------------------------------------------------------------------

newtype IIntMap i a = IIntMap { runIIntMap :: M.IntMap a }
  deriving (Monoid)

-- The type is specialized because I don't know how use coerce polymorphically.
iimFromList1 :: V.Unbox a => [(Index1, a)] -> IIntMap Index1 a
iimFromList1 = IIntMap . M.fromList . coerce

-- The type is specialized because I don't know how use coerce polymorphically.
iimToList1 :: V.Unbox a => IIntMap Index1 a -> [(Index1, a)]
iimToList1 (IIntMap m) = coerce $ M.toList m

iimMapWithKey :: IsInt i => (i -> a -> b) -> IIntMap i a -> IIntMap i b
iimMapWithKey f (IIntMap m) = IIntMap $ M.mapWithKey (f . fromInt) m

iimMember :: IsInt i => i -> IIntMap i a -> Bool
iimMember i (IIntMap m) = M.member (toInt i) m

iimLookup :: IsInt i => i -> IIntMap i a -> Maybe a
iimLookup i (IIntMap m) = M.lookup (toInt i) m

iimUnionWith :: (a -> a -> a) -> IIntMap i a -> IIntMap i a -> IIntMap i a
iimUnionWith f (IIntMap m1) (IIntMap m2) = IIntMap $ M.unionWith f m1 m2
