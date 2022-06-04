module Moodle where


incAll :: [[Int]] -> [[Int]]
incAll list = map (\xs -> map (+1) xs) list


facFoldr :: Integer -> Integer
facFoldr n = foldr (*) 1 [1..n]


sumFoldr :: [Int] -> Int
sumFoldr list = foldr (+) 0 list


concatFoldr :: [[a]] -> [a]
concatFoldr list = foldr (++) [] list


nubFilter :: (Eq a) => [a] -> [a]
nubFilter []     = []
nubFilter (x:xs) = x : filter (/= x) (nubFilter xs)


scalarProduct :: [Int] -> [Int] -> Int
scalarProduct list1 list2 = sumFoldr $ zipWith (*) list1 list2


triples :: Int -> [(Int, Int, Int)]
triples n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n]]


triads :: Int -> [(Int, Int, Int)]
triads n = filter (\(x, y, z) -> x * x + y * y == z * z) $ triples n


nonTrivialTriads :: Int -> [(Int, Int, Int)]
nonTrivialTriads n = nubFilter $ canonicalTraid <$> (filter (\(x, y, z) -> x < y && y < z) $ triads n)
  where
    canonicalTraid :: (Int, Int, Int) -> (Int, Int, Int)
    canonicalTraid (x, y, z) = (xd, yd, zd)
      where
        gcdXYZ = gcd x (gcd y z)
        [xd, yd, zd] = (`div` gcdXYZ) <$> [x, y, z]


indexOf :: Char -> String -> Maybe Int
indexOf c s = foldr comp Nothing (zip s [0..])
  where
    comp :: (Char, Int) -> Maybe Int -> Maybe Int
    comp (el, index) res
      | c == el = Just index
      | otherwise = res


positions :: Char -> String -> [Int]
positions c s = [index | (el, index) <- zip s [0..], el == c]
