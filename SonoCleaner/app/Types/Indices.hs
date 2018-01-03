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
  ( Index0, index0
  , Index1, index1
  , Index2, index2

  , IndexInterval (..)

  , iiLeft
  , iiMember

  , iiDiff
  , iiUndiff

  , IVector
  , ivector

  , ivDiff
  , ivUndiff

  , ivIndex
  , ivSlice

  , ivLength
  , ivEnumFromN
  , ivIndexed
  , ivMap
  , ivZip
  , (*//)
  , ivUpdate

  , ivAverage
  , ivMinMax

  , IIntSet

  , iisOffset

  , iisFromList
  , iisFilter
  ) where

import           Data.Coerce
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
  deriving (Eq, Ord, Enum, Num)

newtype Index1 = Index1 { runIndex1 :: Int }
  deriving (Eq, Ord, Enum, Num)

newtype Index2 = Index2 { runIndex2 :: Int }
  deriving (Eq, Ord, Enum, Num)

index0 :: Int -> Index0
index0 = coerce

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

newtype IndexInterval i = IndexInterval { runIndexInterval :: (i, i) }

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

-- Index conversions

iiDiff :: (IsInt i, IsInt (ISucc i), Enum i)
       => IndexInterval i -> IndexInterval (ISucc i)
iiDiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                              , fromInt $ toInt $ pred u )

iiUndiff :: (IsInt i, IsInt (ISucc i), Enum i)
         => IndexInterval i -> IndexInterval (ISucc i)
iiUndiff (IndexInterval (l, u)) = IndexInterval ( fromInt $ toInt l
                                                , fromInt $ toInt $ succ u )

--------------------------------------------------------------------------------
-- Indexed vectors
--------------------------------------------------------------------------------

-- The individual elements of an `IVector i a` may only be accessed through the
-- use of indices of type `i`. This precludes many functions from the regular
-- Vector API, for example, folds.
newtype IVector i a = IVector { runIVector :: V.Vector a }
  deriving (Monoid)

ivector :: V.Vector a -> IVector i a
ivector = IVector

-- Index conversions

ivDiff :: (Num a, V.Unbox a) => IVector i a -> (a, IVector (ISucc i) a)
ivDiff (IVector v) = (V.head v, IVector $ V.zipWith (-) (V.tail v) v)

ivUndiff :: (Num a, V.Unbox a) => (a, IVector (ISucc i) a) -> IVector i a
ivUndiff (h, (IVector v)) = IVector $ V.scanl' (+) h v

-- Indexing

ivIndex :: (IsInt i, V.Unbox a) => IVector i a -> i -> a
ivIndex (IVector v) i = v V.! toInt i

ivSlice :: (IsInt i, V.Unbox a) => IndexInterval i -> IVector i a -> IVector i a
ivSlice (IndexInterval (i, j)) (IVector v) =
  let i' = toInt i; j' = toInt j
  in  IVector $ V.slice i' (j'-i'+1) v

--------------------------------------------------------------------------------
-- Indexed `Vectors` -- Vector API
--------------------------------------------------------------------------------

ivLength :: (IsInt i, V.Unbox a) => IVector i a -> i
ivLength = fromInt . V.length . coerce

ivEnumFromN :: (IsInt i, Num a, V.Unbox a) =>  a -> i -> IVector i a
ivEnumFromN z i = IVector $ V.enumFromN z (toInt i)

_ivIndices :: (V.Unbox i, Num i, V.Unbox a) => IVector i a -> IVector i i
_ivIndices (IVector v) = IVector $ V.enumFromN 0 (V.length v)

ivIndexed :: (V.Unbox i, Num i, V.Unbox a) => IVector i a -> IVector i (i, a)
ivIndexed iv = ivZip (_ivIndices iv) iv

ivMap :: (V.Unbox a, V.Unbox b) => (a -> b) -> IVector i a -> IVector i b
ivMap f (IVector v) = IVector $ V.map f v

ivZip :: (V.Unbox a, V.Unbox b)
      => IVector i a -> IVector i b -> IVector i (a, b)
ivZip (IVector v) (IVector w) = IVector $ V.zip v w

-- * is just an arbitrary prefix, since 'iv' canont be used.
(*//) :: V.Unbox a => IVector Index1 a -> [(Index1, a)] -> IVector Index1 a
(*//) (IVector v) updates = IVector $ v V.// (coerce updates)

ivUpdate :: V.Unbox a
         => IVector Index1 a -> IVector Index1 (Index1, a) -> IVector Index1 a
ivUpdate (IVector v) (IVector updates) =
  IVector $ V.update v (V.map coerce updates)

-- Folds

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

--------------------------------------------------------------------------------
-- Indexed `IntSet`s
--------------------------------------------------------------------------------

newtype IIntSet i   = IIntSet { runIIntSet :: S.IntSet }
  deriving (Monoid)

_iintset :: S.IntSet -> IIntSet i
_iintset = IIntSet

iisOffset :: IsInt i => i -> IIntSet i -> IIntSet i
iisOffset i (IIntSet s) =
  let i' = toInt i in IIntSet $ S.fromAscList $ map (+i') $ S.toAscList s

--------------------------------------------------------------------------------
-- Indexed `IntSet`s -- IntSet API
--------------------------------------------------------------------------------

-- The type is specialized to Index1 because I don't know how use coerce
-- polymorphically.
iisFromList :: [Index1] -> IIntSet Index1
iisFromList = IIntSet . S.fromList . coerce

iisFilter :: IsInt i => (i -> Bool) -> IIntSet i -> IIntSet i
iisFilter f (IIntSet s) = IIntSet $ S.filter (f . fromInt) s
