module Folds where

import           Prelude hiding (foldl, foldr)


-- rozwiązanie można sprawdzić odpalając `runhaskell FoldsTest.hs`


-- Poniższa funkcja ma działać następująco
-- foldl f z [x1, x2, ..., zn] == (...((z `f` x_1) `f` x_2) ... `f` xn)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f start []     = start
foldl f start (x:xs) = foldl f (start `f` x) xs


-- Poniższa funkcja ma działać następująco:
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z) ... )
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f start []     = start
foldr f start (x:xs) = x `f` (foldr f start xs)


-- Wszystkie poniższe funkcje powinny wywoływać folda i od razu zwracać jego wynik
-- (nie powinny go przetwarzać).

-- Poniższa funkcja używa foldl, żeby policzyć długość listy
lengthFoldl :: [a] -> Int
lengthFoldl = foldl (\ans -> \_ -> ans + 1) 0


-- A poniższa używa foldr:
lengthFoldr :: [a] -> Int
lengthFoldr = foldr (\_ -> \ans -> ans + 1) 0


-- Znajdź takie
strangeF :: Int -> Int -> Int
strangeF = flip const

-- żeby działało inaczej z foldemr i foldeml:
strangeFTest :: Bool
strangeFTest = foldl strangeF 1 [1, 2, 3] /= foldr (flip strangeF) 1 [1, 2, 3]
-- Można użyć hoogla, żeby dowiedzieć się co robi `flip`


-- Poniższa funkcja używa foldl, żeby zaimplementować reverse
reverseFoldl :: [a] -> [a]
reverseFoldl = foldl f []
  where
    f :: [a] -> a -> [a]
    f as x = x : as


-- A poniższa foldr:
reverseFoldr :: [a] -> [a]
reverseFoldr = foldr f []
  where
    f :: a -> [a] -> [a]
    f x as = as ++ [x]


-- Poniższa fukcja używa któregoś z foldów, żeby zaimplementować nub (usuwanie powtórzeń)
nubFold :: (Eq a) => [a] -> [a]
nubFold = foldl f []
  where
    f :: (Eq a) => [a] -> a -> [a]
    f as x
      | x `elem` as = as
      | otherwise = as ++ [x]


-- Poniższa funkcja używa któregoś z foldów, żeby zaimplementować elem :)
elemFold :: (Eq a) => a -> [a] -> Bool
elemFold e = foldl (\result -> \act -> result || act == e) False


-- Poniższa funkcja używa któregoś z foldów, żeby sprawdzić czy dane słowo zawiera "ab" jako podsłowo
containsabFold :: String -> Bool
containsabFold = toBool . foldl consumeChar Start

data State = Start | SeenA | Ok

toBool :: State -> Bool
toBool Ok = True
toBool _  = False

consumeChar :: State -> Char -> State
consumeChar Start 'a' = SeenA
consumeChar Start _   = Start
consumeChar SeenA 'b' = Ok
consumeChar SeenA 'a' = SeenA
consumeChar SeenA _   = Start
consumeChar Ok _      = Ok
