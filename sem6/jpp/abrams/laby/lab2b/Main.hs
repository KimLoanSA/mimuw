module Main where

--- interact :: (String -> String) -> IO ()

f1 :: String -> String
f1 = id


f2 :: String -> String
f2 = reverse

-- 3) Hello World
f3 :: String -> String
f3 = const "Hello World"

-- 4) Liczenie długości wejścia
f4 :: String -> String
f4 = show . length

-- 5) Odwracanie każdego słowa z wejścia
f5 :: String -> String
f5 = unwords . map reverse . words

-- 5b) Odwracanie każej linii z wejścia
f5b :: String -> String
f5b = unlines . map reverse . lines

-- 6*) Nieinteraktywna identyczność -- wypisujemy wszystko na koniec
f6 :: String -> String
f6 = reverse . reverse

-- 7*) Identyczność, która wyświetla wszsystko co dwie linijki
f7 :: String -> String
f7 = unlines . id2 . lines
  where
    id2 :: [a] -> [a]
    id2 []             = []
    id2 [x]            = [x]
    id2 (x1 : x2 : xs) = x1 : x2 : id2 xs

-- 8) Zadanie 8 z moodla (ignorując newline'y z wejścia)
f8 :: String -> String
f8 = unlines . map unwords . splitInput 10 . words

-- 9) Zadanie 8 z moodla (respektując newline'y z wejścia)
f9 :: String -> String
f9 = unlines . map f8 . lines


splitInput :: Int -> [String] -> [[String]]
splitInput maxSize input = addLine $ foldl addWord ([], []) $ concat $ map (splitString maxSize) input
  where
    addWord :: ([String], [[String]]) -> String -> ([String], [[String]])
    addWord (currentLine, allLines) nextWord
        | length (unwords $ nextWord : currentLine) <= maxSize = (currentLine ++ [nextWord], allLines)
        | otherwise = ([nextWord], addLine (currentLine, allLines))

    addLine :: ([String], [[String]]) -> [[String]]
    addLine ([], allLines)          = allLines
    addLine (currentLine, allLines) = allLines ++ [currentLine]

    splitString :: Int -> String -> [String]
    splitString maxSize [] = []
    splitString maxSize s = take maxSize s : (splitString maxSize $ drop maxSize s)


splitString :: Int -> String -> [String]
splitString maxSize [] = []
splitString maxSize s  = take maxSize s : (splitString maxSize $ drop maxSize s)


main :: IO ()
main = interact f9
