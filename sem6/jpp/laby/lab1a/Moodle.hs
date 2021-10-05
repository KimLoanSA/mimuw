module Moodle where

import           Prelude hiding (drop, filter, head, map, tail, take, (++))


head :: [a] -> a
head (x:_) = x


tail :: [a] -> [a]
tail (_:xs) = xs


(++) :: [a] -> [a] -> [a]
(++) [] ys     = ys
(++) (a:xs) bs = a : xs ++ bs


take :: Int -> [a] -> [a]
take n []     = []
take n (x:xs) = if n > 0 then x : (take (n - 1) xs) else []


drop :: Int -> [a] -> [a]
drop n []     = []
drop n (x:xs) = if (n <= 0) then x : xs else drop (n - 1) xs


map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : (map f xs)


filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs


inits :: [a] -> [[a]]
inits as = initsHelp as []

initsHelp :: [a] -> [a] -> [[a]]
initsHelp [] as    = [as]
initsHelp (a:as) b = [b] ++ (initsHelp as (b ++ [a]))


tails :: [a] -> [[a]]
tails as = tailsHelp as as

tailsHelp :: [a] -> [a] -> [[a]]
tailsHelp [] as         = [as]
tailsHelp (_:as) (b:bs) = [b:bs] ++ (tailsHelp as bs)


partitions :: [a] -> [([a], [a])]
partitions as = partitionsHelp [] as

partitionsHelp :: [a] -> [a] -> [([a], [a])]
partitionsHelp as []     = [(as, [])]
partitionsHelp as (b:bs) = [(as, b:bs)] ++ partitionsHelp (as ++ [b]) bs


permutations :: [a] -> [[a]]
permutations (a:[]) = [[a]]
permutations (a:as) = permutationsAdd a (permutations as)

permutationsAdd :: a -> [[a]] -> [[a]]
permutationsAdd x []       = []
permutationsAdd x (as:ass) = (permutationAdd x [] as) ++ (permutationsAdd x ass)

permutationAdd :: a -> [a] -> [a] -> [[a]]
permutationAdd x as [] = [as ++ [x]]
permutationAdd x as (b:bs) = [as ++ [x] ++ [b] ++ bs] ++ (permutationAdd x (b:as) bs)


nub :: Eq a => [a] -> [a]
nub as = nubHelp as []

nubHelp :: Eq a => [a] -> [a] -> [a]
nubHelp [] _ = []
nubHelp (a:as) bs = if a `elem` bs then nubHelp as bs else a : nubHelp as (a : bs)
