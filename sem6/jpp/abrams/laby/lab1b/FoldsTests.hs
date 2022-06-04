module FoldsTests where

import           Prelude hiding (foldl, foldr)

import           Folds

tests :: [Bool]
tests = [
    foldr (^) 2 [1, 2, 3] == 1,
    foldl (^) 2 [1, 2, 3] == 64,

    lengthFoldl [1, 2, 3] == 3,
    lengthFoldr [1, 2, 3] == 3,

    strangeFTest,

    reverseFoldl [1, 2, 3] == [3, 2, 1],
    reverseFoldr [1, 2, 3] == [3, 2, 1],

    nubFold [1,2,1,3,1,2,1,4] == [1,2,3,4],

    1 `elemFold` [5, 1, 5],
    not $ 2 `elemFold` [5, 1, 5],

    containsabFold "ab",
    containsabFold "baaaabaa",
    not $ containsabFold "baaaaaa",
    containsabFold "ccadabcda",
    not $ containsabFold "accccb"]

testsStr :: [String]
testsStr = [
    "foldr (^) 2 [1, 2, 3] == 1",
    "foldl (^) 2 [1, 2, 3] == 64",

    "lengthFoldl [1, 2, 3] == 3",
    "lengthFoldr [1, 2, 3] == 3",

    "strangeFTest",

    "reverseFoldl [1, 2, 3] == [3, 2, 1]",
    "reverseFoldr [1, 2, 3] == [3, 2, 1]",

    "nubFold [1,2,1,3,1,2,1,4] == [1,2,3,4]",

    "1 `elemFold` [5, 1, 5]",
    "not $ 2 `elemFold` [5, 1, 5]",

    "containsabFold \"ab\"",
    "containsabFold \"baaaabaa\"",
    "not $ containsabFold \"baaaaaa\"",
    "containsabFold \"ccadabcda\"",
    "not $ containsabFold \"accccb\""]

testsWrong :: [String]
testsWrong = map snd $ filter (not . fst) $ zip tests testsStr


main :: IO ()
main = if not $ null testsWrong then
    putStrLn "The following tests failed:" >> mapM_ putStrLn testsWrong
    else putStrLn "OK"
