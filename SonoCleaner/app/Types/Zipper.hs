-- Implementation of a basic Zipper data structure

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes        #-}

module Types.Zipper
  ( Zipper
  , extract
  , nonFocused
  , rights
  , lefts
  , rightIx
  , leftIx
  , right
  , left
  , tugLeft
  , tugRight
  , fromList
  , unsafeFromList
  , fromNonEmpty
  , head
  , last
  , clobberRight
  , collapseRight
  , dropRight
  , dropLeft
  ) where

import           Prelude            hiding (head, last, null)

import           Control.Lens
import qualified Data.List          as L
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Monoid

--------------------------------------------------------------------------------
-- Zipper data type
--------------------------------------------------------------------------------

data Zipper a = Zipper [a] a [a]
  deriving (Eq, Functor, Traversable)

instance Foldable Zipper where
  foldMap f (Zipper ls c rs) = foldMap f (reverse ls) <> f c <> foldMap f rs

--------------------------------------------------------------------------------
-- Zipper manipulation
--------------------------------------------------------------------------------

-- We use simple lenses to preserve the element type.

extract :: Lens' (Zipper a) a
extract f (Zipper ls c rs) = flip (Zipper ls) rs <$> f c

nonFocused :: Traversal' (Zipper a) a
nonFocused f (Zipper ls c rs) =
  Zipper <$> fmap reverse (traverse f (reverse ls)) <*> pure c <*> traverse f rs

rights :: Lens' (Zipper a) [a]
rights f (Zipper ls c rs) = Zipper ls c <$> f rs

lefts :: Lens' (Zipper a) [a]
lefts f (Zipper ls c rs) = (\ls' -> Zipper ls' c rs) <$> f ls

rightIx :: Int -> Traversal' (Zipper a) a
rightIx n = rights . ix n

leftIx :: Int -> Traversal' (Zipper a) a
leftIx  n = lefts  . ix n

right :: Traversal' (Zipper a) a
right = rightIx 0

left :: Traversal' (Zipper a) a
left = leftIx 0

tugLeft :: Zipper a -> Zipper a
tugLeft z@(Zipper [] _ _)    = z
tugLeft (Zipper (l:ls) c rs) = Zipper ls l (c:rs)

tugRight :: Zipper a -> Zipper a
tugRight z@(Zipper _ _ [])    = z
tugRight (Zipper ls c (r:rs)) = Zipper (c:ls) r rs

fromList :: [a] -> Maybe (Zipper a)
fromList []     = Nothing
fromList (x:xs) = Just $ Zipper [] x xs

unsafeFromList :: [a] -> Zipper a
unsafeFromList []     = error "unsafeFromList: Zippers must be non-empty."
unsafeFromList (x:xs) = Zipper [] x xs

fromNonEmpty :: NonEmpty a -> Zipper a
fromNonEmpty (x :| xs) = Zipper [] x xs

head :: Lens' (Zipper a) a
head f (Zipper [] c rs) = (\c' -> Zipper [] c' rs) <$> f c
head f (Zipper ls c rs) = ((\ls' -> Zipper ls' c rs) . (L.init ls ++) . (:[]))
                        <$> f (L.last ls)

last :: Lens' (Zipper a) a
last f (Zipper ls c []) = (\c' -> Zipper ls c' []) <$> f c
last f (Zipper ls c rs) = (Zipper ls c . (rs'++) . (:[])) <$> f r
  where rs' = L.init rs
        r   = L.last rs

clobberRight :: a -> Zipper a -> Zipper a
clobberRight a (Zipper ls c _) = Zipper (c:ls) a []

collapseRight :: Zipper a -> Maybe (Zipper a)
collapseRight (Zipper ls _ (r:rs)) = Just $ Zipper ls r rs
collapseRight (Zipper (l:ls) _ []) = Just $ Zipper ls l []
collapseRight (Zipper []     _ []) = Nothing

dropRight :: Int -> Zipper a -> Zipper a
dropRight n (Zipper ls c rs) = Zipper ls c (drop n rs)

dropLeft :: Int -> Zipper a -> Zipper a
dropLeft n (Zipper ls c rs) = Zipper (drop n ls) c rs
