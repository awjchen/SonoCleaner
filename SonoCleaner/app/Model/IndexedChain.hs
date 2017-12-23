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
import           Control.Monad.Primitive
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

--------------------------------------------------------------------------------

newtype IndexedChain s a = IndexedChain (VM.MVector s (Bool, ElemIndex, a))

type ElemIndex = Int

nullIndex :: ElemIndex
nullIndex = 0

--------------------------------------------------------------------------------

fromList :: (VM.Unbox a, PrimMonad m) => [a] -> m (IndexedChain (PrimState m) a)
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
next
  :: (VM.Unbox a, PrimMonad m)
  => IndexedChain (PrimState m) a
  -> ElemIndex
  -> m (Maybe (ElemIndex, a))
next chain@(IndexedChain v) i = do
  (_, nextIndex, _) <- VM.unsafeRead v i
  next' chain [] i nextIndex

next'
  :: (VM.Unbox a, PrimMonad m)
  => IndexedChain (PrimState m) a
  -> [ElemIndex]
  -> ElemIndex
  -> ElemIndex
  -> m (Maybe (ElemIndex, a))
next' chain@(IndexedChain v) badQueries i nextIndex =
  let update = forM_ badQueries $ \j -> VM.unsafeModify v (set _2 nextIndex) j
  in  if nextIndex == nullIndex
      then update >> return Nothing
      else do
        (nextExists, nextNextIndex, nextVal) <- VM.unsafeRead v nextIndex
        if nextExists
        then update >> return (Just (nextIndex, nextVal))
        else next' chain (i:badQueries) nextIndex nextNextIndex

remove :: (VM.Unbox a, PrimMonad m)
       => IndexedChain (PrimState m) a -> ElemIndex -> m ()
remove (IndexedChain v) = VM.unsafeModify v (set _1 False)

query :: (VM.Unbox a, PrimMonad m)
      => IndexedChain (PrimState m) a -> ElemIndex -> m (Maybe a)
query (IndexedChain v) i = do
  (exists, _, val) <- VM.unsafeRead v i
  return $ if exists then Just val else Nothing
