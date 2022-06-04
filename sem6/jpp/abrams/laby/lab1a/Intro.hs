module Intro where

import           Prelude hiding (Just, Maybe, Nothing)


-- Enumeration Types:

data Direction = North | East | West | South
  deriving Show

turnClockwise :: Direction -> Direction
turnClockwise North = East
turnClockwise East  = South
turnClockwise South = West
turnClockwise West  = North

turnAround :: Direction -> Direction
turnAround = turnClockwise . turnClockwise


-- Union Types:

data Shape = Rectangle Double Double
           | Circle Double
           | Triangle Double Double Double
           deriving Show

area :: Shape -> Double
area (Rectangle x y) = x * y
area (Circle r) = 3.14 * r * r
area (Triangle a b c) = sqrt toRoot
  where
    p = (a + b + c) / 2
    toRoot = p * (p - a) * (p - b) * (p - c)

perimeter :: Shape -> Double
perimeter (Rectangle x y)  = (x + y) * 2
perimeter (Circle r)       = 2 * 3.14 * r
perimeter (Triangle a b c) = a + b + c


-- Recursive Types:

data Nat = Z | S Nat
  deriving (Eq, Show)

o :: Nat
o = Z

i :: Nat
i = (S Z)

ii :: Nat
ii = (S (S Z))

iii :: Nat
iii = (S (S (S Z)))

add :: Nat -> Nat -> Nat
add Z x     = x
add (S y) x = S (add y x)

isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S x)) = isEven x

isOdd :: Nat -> Bool
isOdd = not . isEven

mult :: Nat -> Nat -> Nat
mult Z _     = Z
mult (S x) y = add y (mult x y)


-- Generic Types:

data Maybe x = Just x | Nothing
  deriving Show
-- Wartość lub jej brak

safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just x) (Just y) = Just (x + y)
addMaybe _ _               = Nothing


-- Generic, recursive types

data MyList a = Nil | Cons a (MyList a)

-- data [a] = [] | a:[a]

l1 :: [Int]
l1 = 1:(2:(3:[]))

l2 :: [Int]
l2 = [1, 2, 3]

l3 :: MyList Int
l3 = Cons 1 (Cons 2 (Cons 3 Nil))

l4 :: MyList Int
l4 = 1 `Cons` (2 `Cons` Nil)

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

myMyLength :: MyList a -> Nat
myMyLength Nil         = Z
myMyLength (Cons _ xs) = S (myMyLength xs)

myMyLengthTest :: Bool
myMyLengthTest = myMyLength l3 == (S (S (S Z)))

listToMyList :: [a] -> MyList a
listToMyList []     = Nil
listToMyList (x:xs) = Cons x (listToMyList xs)

myListToList :: MyList a -> [a]
myListToList Nil         = []
myListToList (Cons s xs) = s : (myListToList xs)

myFilter :: (a -> Bool) -> ([a] -> [a])
myFilter f []     = []
myFilter f (x:xs) = if f x then x : myFilter f xs else myFilter f xs

-- Częściowo oparty na Moodlu przedmiotu, częściowo na rozdziale 4 z książki: Edward Brady, Type-driven development with Idris
