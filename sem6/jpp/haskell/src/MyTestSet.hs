module MyTestSet where

import Set
import Prelude hiding(null)


emptySet :: Set Int
emptySet = empty

singletonSet :: Set Int
singletonSet = singleton 42

set1 :: Set Int
set1 = fromList [1, 2, 3]

set2 :: Set Int
set2 = fromList [2, 1, 3, 3, 3, 7, 2]

set3 :: Set Int
set3 = fromList [42]

setEmptyEmpty :: Set Int
setEmptyEmpty = union emptySet empty

setEmpty1 :: Set Int
setEmpty1 = union emptySet set1

setSingleton2 :: Set Int
setSingleton2 = union set2 singletonSet

set12 :: Set Int
set12 = union set1 set2

setEmptyIns :: Set Int
setEmptyIns = insert 69 emptySet

setSingletonIns :: Set Int
setSingletonIns = insert 69 singletonSet

set1Ins :: Set Int
set1Ins = insert 69 set1

set2Ins :: Set Int
set2Ins = insert 7 set2

setEmptyPlus :: Set Int
setEmptyPlus = (+1) <$> emptySet

setSingletonPlus :: Set Int
setSingletonPlus = (+1) <$> singletonSet

set2Plus :: Set Int
set2Plus = (+1) <$> set2


tests :: [Bool]
tests = [
  null emptySet      == True,
  null singletonSet  == False,
  null set1          == False,
  null setEmptyEmpty == True,

  member 42 emptySet        == False,
  member 43 singletonSet    == False,
  member 42 singletonSet    == True,
  member 42 set1            == False,
  member 2  set1            == True,
  member 42 set2            == False,
  member 2  set2            == True,
  member 7  set2            == True,
  member 42 set3            == True,
  member 43 set3            == False,
  member 42 setEmptyEmpty   == False,
  member 2  setEmpty1       == True,
  member 43 setEmpty1       == False,
  member 42 setSingleton2   == True,
  member 43 setSingleton2   == False,
  member 2  set12           == True,
  member 42 set12           == False,
  member 69 setEmptyIns     == True,
  member 69 setSingletonIns == True,
  member 69 set1Ins         == True,
  member 7  set2Ins         == True,

  toList emptySet      == [],
  toList singletonSet  == [42],
  toList set1          == [1, 2, 3],
  toList set2          == [2, 1, 3, 3, 3, 7, 2],
  toList set3          == [42],
  toList setEmptyEmpty == [],
  toList setEmpty1     == [1, 2, 3],

  toAscList emptySet         == [],
  toAscList singletonSet     == [42],
  toAscList set1             == [1, 2, 3],
  toAscList set2             == [1, 2, 3, 7],
  toAscList set3             == [42],
  toAscList setEmptyEmpty    == [],
  toAscList setEmpty1        == [1, 2, 3],
  toAscList setSingleton2    == [1, 2, 3, 7, 42],
  toAscList set12            == [1, 2, 3, 7],
  toAscList setEmptyIns      == [69],
  toAscList setSingletonIns  == [42, 69],
  toAscList set1Ins          == [1, 2, 3, 69],
  toAscList set2Ins          == [1, 2, 3, 7],
  toAscList setEmptyPlus     == [],
  toAscList setSingletonPlus == [43],
  toAscList set2Plus         == [2, 3, 4, 8],

  emptySet      == emptySet,
  setEmptyEmpty == emptySet,
  set12         == set2,
  set1          /= set2
  ]


testsStr :: [String]
testsStr = [
  "null emptySet      == True",
  "null singletonSet  == False",
  "null set1          == False",
  "null setEmptyEmpty == True", 

  "member 42 emptySet      == False",
  "member 43 singletonSet  == False",
  "member 42 singletonSet  == True",
  "member 42 set1          == False",
  "member 2  set1          == True",
  "member 42 set2          == False",
  "member 2  set2          == True",
  "member 7  set2          == True",
  "member 42 set3          == True",
  "member 43 set3          == False",
  "member 42 setEmptyEmpty == False",
  "member 2  setEmpty1     == True",
  "member 43 setEmpty1     == False",
  "member 42 setSingleton2 == True",
  "member 43 setSingleton2 == False",
  "member 2  set12         == True",
  "member 42 set12         == False",
  "member 69 setEmptyIns     == True",
  "member 69 setSingletonIns == True",
  "member 69 set1Ins         == True",
  "member 7  set2Ins         == True",

  "toList emptySet      == []",
  "toList singletonSet  == [42]",
  "toList set1          == [1, 2, 3]",
  "toList set2          == [2, 1, 3, 3, 3, 7, 2]",
  "toList set3          == [42]",
  "toList setEmptyEmpty == []",
  "toList setEmpty1     == [1, 2, 3]",

  "toAscList emptySet      == []",
  "toAscList singletonSet  == [42]",
  "toAscList set1          == [1, 2, 3]",
  "toAscList set2          == [1, 2, 3, 7]",
  "toAscList set3          == [42]",
  "toAscList setEmptyEmpty == []",
  "toAscList setEmpty1     == [1, 2, 3]",
  "toAscList setSingleton2 == [1, 2, 3, 7, 42]",
  "toAscList set12         == [1, 2, 3, 7]",
  "toAscList setEmptyIns     == [69]",
  "toAscList setSingletonIns == [42, 69]",
  "toAscList set1Ins         == [1, 2, 3, 69]",
  "toAscList set2Ins         == [1, 2, 3, 7]",
  "toAscList setEmptyPlus     == []",
  "toAscList setSingletonPlus == [43]",
  "toAscList set2Plus        == [2, 3, 4, 8]",

  "emptySet      == emptySet",
  "setEmptyEmpty == emptySet",
  "set12         == set2",
  "set1          /= set2"
  ]


testsWrong :: [String]
testsWrong = map snd $ filter (not . fst) $ zip tests testsStr


main :: IO ()
main = if length testsWrong > 0 then 
  putStrLn "The following tests failed:" >> mapM_ putStrLn testsWrong 
  else putStrLn "OK" 
