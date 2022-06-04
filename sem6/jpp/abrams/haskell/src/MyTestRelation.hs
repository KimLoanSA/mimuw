module MyTestBasic where

import Graph
import Set


graph11 :: Relation Int
graph11 = (fromInteger 1) * (fromInteger 1)

graph12 :: Relation Int
graph12 = (fromInteger 1) + (fromInteger 2)

graph34 :: Relation Int
graph34 = (fromInteger 3) + (fromInteger 4)

graph56 :: Relation Int
graph56 = Relation { domain = fromList [5, 6], relation = fromList [(5, 6)] }


graph12plusGraph34         = Relation { domain = fromList [1, 2, 3, 4], relation = fromList [] }
graph12xGraph34            = Relation { domain = fromList [1, 2, 3, 4], relation = fromList [(1, 3), (2, 3), (1, 4), (2, 4)] }

graph56plusGraph12         = Relation { domain = fromList [1, 2, 5, 6], relation = fromList [(5, 6)] }
graph56xGraph12            = Relation { domain = fromList [1, 2, 5, 6], relation = fromList [(5, 6)] }

graph56plusgraph12xGraph34 = Relation { domain = fromList [1, 2, 3, 4, 5, 6], relation = fromList [(1, 3), (2, 3), (1, 4), (2, 4), (5, 6)] }
graph56xgraph12xGraph34    = Relation { domain = fromList [1, 2, 3, 4, 5, 6], relation = fromList [(1, 3), (2, 3), (1, 4), (2, 4),
                                                                                                   (5, 1), (5, 2), (5, 3), (5, 4), (5, 6),
                                                                                                   (6, 1), (6, 2), (6, 3), (6, 4)] }


tests :: [Bool]
tests = [
    graph12 + graph34         == graph12plusGraph34,
    graph12 * graph34         == graph12xGraph34,

    graph56 + graph12         == graph56plusGraph12,
    graph56 + graph12         == graph56xGraph12,

    graph56 + graph12xGraph34 == graph56plusgraph12xGraph34,
    graph56 * graph12xGraph34 == graph56xgraph12xGraph34,

    graph11                   == Relation { domain = fromList [1], relation = fromList [(1, 1)]}
  ]


testsStr :: [String]
testsStr = [
    "graph12 + graph34         == graph12plusGraph34",
    "graph12 * graph34         == graph12xGraph34",

    "graph56 + graph12         == graph56plusGraph12",
    "graph56 + graph12         == graph56xGraph12",

    "graph56 + graph12xGraph34 == graph56plusgraph12xGraph34",
    "graph56 * graph12xGraph34 == graph56xgraph12xGraph34",

    "graph11                   == Relation { domain = fromList [1], relation = fromList [(1, 1)]}"
  ]


testsWrong :: [String]
testsWrong = map snd $ filter (not . fst) $ zip tests testsStr

main :: IO ()
main = if length testsWrong > 0 then 
  putStrLn "The following tests failed:" >> mapM_ putStrLn testsWrong 
  else putStrLn "OK"
