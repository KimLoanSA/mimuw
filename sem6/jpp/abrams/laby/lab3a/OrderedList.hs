module OrderedList(OrderedList, fromList, toList, insert, remove) where

import qualified Data.List   as L
import           Debug.Trace


newtype OrderedList a = OL [a]
  deriving (Show, Eq)


instance Functor OrderedList where
  fmap f (OL x) = OL $ f <$> x


-- musi zachodzić
-- (a <> (b <> c)) == ((a <> b) <> c)
instance (Ord a) => Semigroup (OrderedList a) where
  (OL x) <> (OL y) = OL $ merge x y
    where
      merge []     y      = y
      merge x      []     = x
      merge (x:xs) (y:ys)
        | x < y           = x : (merge xs (y:ys))
        | otherwise       = y : (merge (x:xs) ys)


-- musi zachodzić
-- (mempty <> a) == (a <> mempty)
-- (a <> mempty) == a
instance (Ord a) => Monoid (OrderedList a) where
  mempty = OL []


fromList :: (Ord a) => [a] -> OrderedList a
fromList = OL . L.sort


toList :: OrderedList a -> [a]
toList (OL x) = x


nubOrdered :: Ord a => OrderedList a -> OrderedList a
nubOrdered (OL xs) = OL $ nubOrderedHelp xs Nothing
  where
    nubOrderedHelp []     _           = []
    nubOrderedHelp (x:xs) Nothing     = x : nubOrderedHelp xs (Just x)
    nubOrderedHelp (x:xs) (Just last)
      | last == x                     = nubOrderedHelp xs (Just x)
      | otherwise                     = x : nubOrderedHelp xs (Just x)


-- Insert -- O(n)
insert :: (Ord a) => OrderedList a -> a -> OrderedList a
insert (OL x) elem = OL $ insertHelp x elem
  where
    insertHelp [] elem     = [elem]
    insertHelp (x:xs) elem
      | x < elem           = x : insertHelp xs elem
      | otherwise          = elem : x : xs


-- Remove -- O(n)
remove :: (Ord a) => OrderedList a -> a -> OrderedList a
remove (OL x) elem = OL $ removeAux x elem
  where
    removeAux [] elem = []
    removeAux (x:xs) elem
      | x >= elem = x:xs
      | x == elem = xs
      | otherwise = x : removeAux xs elem
-- Debugging
