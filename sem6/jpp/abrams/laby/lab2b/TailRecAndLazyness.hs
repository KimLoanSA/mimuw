module TailRecAndLazyness where

import           Prelude hiding (reverse, take)


----------------------
-- Rekursja ogonowa --
----------------------

fact :: Int -> Int
fact n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = n * fact(n - 1)


tailFact :: Int -> Int
tailFact n = tailFactAux 1 n
  where
    tailFactAux acc 0 = acc
    tailFactAux acc n = tailFactAux (n*acc) (n-1)


myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]


myReverseTail :: [a] -> [a]
myReverseTail l = myReverseTailAux [] l
  where
    myReverseTailAux acc []     = acc
    myReverseTailAux acc (x:xs) = myReverseTailAux (x:acc) xs


-- Do pomyślenia: liczby Fibbonacciego ogonowo


--------------
-- Leniwość --
--------------

--- Haskellowa definicja ---
reverse :: [a] -> [a]
reverse l =  rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)


-- Take
take :: Int -> [a] -> [a]
take _ [] = []
take n (x:xs)
    | n <= 0 = []
    | otherwise = x : take (n-1) xs


-- "nieinteraktywna" wersja take
take2 :: Int -> [a] -> [a]
take2 = takeAux []
  where
    takeAux :: [a] -> Int -> [a] -> [a]
    takeAux ans _ [] = reverse ans
    takeAux ans n (x:xs)
      | n <= 0 = takeAux ans (n-1) xs
      | otherwise = takeAux (x:ans) (n-1) xs


fives :: [Int]
fives = 5 : fives

-- take 7 fives
-- take 7 $ reverse fives
-- take2 7 fives


mystery :: [Int]
mystery = mystery

-- take 7 mystery


allInts :: [Int]
allInts = 1 : map (+1) allInts

-- take 7 allInts


-- Do pomyślenia:

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


primes :: [Integer]
primes = sieve [2..]
  where
    sieve (prime:xs) = prime : sieve [x | x <- xs, x `mod` prime > 0]
