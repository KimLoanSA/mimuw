{-# LANGUAGE RecordWildCards #-}

module DataStructures where

import qualified Data.Map as M
import qualified Data.Set as S


--------------------
-- Stos i Kolejka --
--------------------

newtype Stack a = Stack [a]

makeEmptyStack :: Stack a
makeEmptyStack = Stack []


topS :: Stack a -> Maybe a
topS (Stack [])     = Nothing
topS (Stack (x:xs)) = Just x


nullS :: Stack a -> Bool
nullS (Stack []) = True
nullS _          = False


popS :: Stack a -> Stack a
popS (Stack [])     = Stack []
popS (Stack (x:xs)) = Stack xs


pushS :: Stack a -> a -> Stack a
pushS (Stack old) elem = Stack (elem:old)


-- Dla chętnych: kolejka za pomocą dwóch stosów

data Queue a = Queue {
  qIn  :: Stack a,
  qOut :: Stack a
  }

-- qIn :: Queue a -> Stack a
-- qOut :: Queue a -> Stack a

-- emptyQ :: Queue a -> Bool
-- emptyQ Queue {..} = (nullS qIn) && (nullS qOut)

-- emptyQ (Queue q1 q2) = undefined

-- Można poczytać:
-- Record Wildcars:
-- emptyQ Queue{..} = undefined
-- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html


---------------
-- Set i Map --
---------------

-- dokumentacja do Data.Map znajduje sie w module Data.Map.Strict (są takie same).
-- https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html
-- (można też wpisać Data.Map.Struct w hoogla)

-- dokumentacja Data.Set
-- https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html
-- (można też wpisać Data.Set w hoogla)


-- zastąp każdy napis z listy przez indeks jego pierwszego
-- pojawienia się na liście (numerujemy od zera)
-- renumerate ["a", "ba", "b", "ba", "a"] = [0, 1, 2, 1, 0]

renumerate :: [String] -> [Int]
renumerate list = renumerateHelp (zip list [0..]) M.empty
  where
    renumerateHelp :: [(String, Int)] -> (M.Map String Int) -> [Int]
    renumerateHelp [] _ = []
    renumerateHelp ((text, index):xs) mapping =
      case M.lookup text mapping of
        Just n -> n : (renumerateHelp xs mapping)
        Nothing -> let newMapping = M.insert text index mapping
                   in index : (renumerateHelp xs newMapping)


-- normalnie w Data.Containers.ListUtils
-- nub w O(n log n)
nubOrd :: (Ord a) => [a] -> [a]
nubOrd list = reverse $ fst $ foldl f ([], S.empty) list
  where
    f :: (Ord a) => ([a], S.Set a) -> a -> ([a], S.Set a)
    f (list, set) elem
      | elem `S.member` set = (list, set)
      | otherwise = (elem:list, S.insert elem set)


-- sprawdzamy czy można tak ułożyć literki w napisie `x`
-- (być może niektóre usuwając), by uzyskać napis `y`. O(n log n)
canBeObtainedFrom :: String -> String -> Bool
canBeObtainedFrom goal text = M.foldr f True diffMap
  where
    goalMap :: M.Map Char Int
    goalMap = makeMap goal

    textMap :: M.Map Char Int
    textMap = makeMap text

    makeMap :: String -> M.Map Char Int
    makeMap s = M.fromListWith (+) [(c, 1) | c <- s]

    diffMap :: M.Map Char Int
    diffMap = M.differenceWith (\left -> \right -> nothingIf (<= 0) (left - right)) goalMap textMap

    nothingIf :: (a -> Bool) -> a -> Maybe a
    nothingIf f a
      | f a = Nothing
      | otherwise = Just a

    f :: Int -> Bool -> Bool
    f _ False = False
    f val _
      | val >= 0 = False
      | otherwise = True


---------------
-- Funktory ---
---------------

t1 = (+1) <$> [1, 2, 3]

t2 = (+1) <$> Just 5
t3 = (+1) <$> Nothing


f :: String -> Int
f "Jeden" = 1
f "Dwa"   = 2
f "Trzy"  = 3
f _       = 0

t4 = (+1) <$> f

-- Dla chętnych : Functors.hs
