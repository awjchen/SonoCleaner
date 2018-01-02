-- A linked-list implemented as an array. Elements can only be queried and
-- removed once the data structure is initialized, but we can index, remove, and
-- find the next element in O(1) (amortized) time.

module Model.IndexedChain
  ( IndexedChain
  , ElemIndex
  , fromList
  , next
  , remove
  , query
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

--------------------------------------------------------------------------------

newtype IndexedChain s a = IndexedChain (VM.MVector s (Bool, ElemIndex, a))

type ElemIndex = Int

nullIndex :: ElemIndex
nullIndex = 0

--------------------------------------------------------------------------------

{-# INLINEABLE fromList #-}
fromList :: (VM.Unbox a) => [a] -> ST s (IndexedChain s a)
fromList xs =
  let values = V.fromList xs
      n = V.length values
      links  = V.generate n succ
      exists = V.replicate n True
  in  do
    chain <- V.unsafeThaw $ V.zip3 exists links values
    unless (null xs)
      $ VM.unsafeModify chain (set _2 nullIndex) (n-1)
    return $ IndexedChain chain

-- Finds the least index greater than the provided index.
{-# INLINEABLE next #-}
next
  :: (VM.Unbox a)
  => IndexedChain s a
  -> ElemIndex
  -> ST s (Maybe (ElemIndex, a))
next chain@(IndexedChain v) i = do
  (_, nextIndex, _) <- VM.unsafeRead v i
  next' chain [] i nextIndex

next'
  :: (VM.Unbox a)
  => IndexedChain s a
  -> [ElemIndex]
  -> ElemIndex
  -> ElemIndex
  -> ST s (Maybe (ElemIndex, a))
next' chain@(IndexedChain v) badQueries i nextIndex =
  let update = forM_ badQueries $ VM.unsafeModify v (set _2 nextIndex)
  in  if nextIndex == nullIndex
      then update >> return Nothing
      else do
        (nextExists, nextNextIndex, nextVal) <- VM.unsafeRead v nextIndex
        if nextExists
        then update >> return (Just (nextIndex, nextVal))
        else next' chain (i:badQueries) nextIndex nextNextIndex

{-# INLINEABLE remove #-}
remove :: (VM.Unbox a) => IndexedChain s a -> ElemIndex -> ST s ()
remove (IndexedChain v) = VM.unsafeModify v (set _1 False)

{-# INLINEABLE query #-}
query :: (VM.Unbox a) => IndexedChain s a -> ElemIndex -> ST s (Maybe a)
query (IndexedChain v) i = do
  (exists, _, val) <- VM.unsafeRead v i
  return $ if exists then Just val else Nothing
