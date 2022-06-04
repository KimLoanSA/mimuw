module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems
              ) where
import Prelude hiding(null)
import Data.List (nub, sort)



data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)



empty :: Set a
empty = Empty


null :: Set a -> Bool
null Empty = True
null _     = False


member :: Eq a => a -> Set a -> Bool
member _ Empty              = False
member x (Singleton setVal) = x == setVal
member x (Union set1 set2)  = member x set1 || member x set2

singleton :: a -> Set a
singleton = Singleton


fromList :: [a] -> Set a
fromList = foldr (Union . Singleton) Empty


toList :: Set a -> [a]
toList Empty             = []
toList (Singleton val)   = [val]
toList (Union set1 set2) = toList set1 ++ toList set2


toAscList :: Ord a => Set a -> [a]
toAscList set = unique Nothing $ sort $ toList set
  where
    unique _ [] = []
    unique Nothing (x:xs) = x : unique (Just x) xs
    unique (Just prev) (x:xs)
      | prev == x         = unique (Just x) xs
      | otherwise         = x : unique (Just x) xs


elems :: Set a -> [a]
elems = toList


union :: Set a -> Set a -> Set a
union Empty Empty = Empty
union Empty set2  = set2
union set1  Empty = set1
union set1  set2  = Union set1 set2


insert :: a -> Set a -> Set a
insert x = union (Singleton x)



instance Ord a => Eq (Set a) where
  set1 == set2 = toAscList set1 == toAscList set2


instance Semigroup (Set a) where
  set1 <> set2 = set1 `union` set2


instance Monoid (Set a) where
  mempty = Empty


instance Show a => Show (Set a) where
  show set1 = show $ toList set1


instance Functor Set where
  fmap f Empty             = Empty
  fmap f (Singleton x)     = Singleton (f x)
  fmap f (Union set1 set2) = Union (f <$> set1) (f <$> set2)
