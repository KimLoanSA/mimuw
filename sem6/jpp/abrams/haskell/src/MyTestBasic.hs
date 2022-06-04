module MyTestBasic where

import Graph
import Set

graph11 :: Basic Int
graph11 = (fromInteger 1) * (fromInteger 1)

graph12 :: Basic Int
graph12 = (fromInteger 1) + (fromInteger 2)

graph34 :: Basic Int
graph34 = (fromInteger 3) + (fromInteger 4)

graph56 :: Basic Int
graph56 = (fromInteger 5) * (fromInteger 6)

graph1234 :: Basic Int
graph1234 = ((fromInteger 1) * (fromInteger 2)) + ((fromInteger 2) * (fromInteger 3)) + ((fromInteger 3) * (fromInteger 4))

graph56plusgraph12xGraph34 :: Relation Int
graph56plusgraph12xGraph34 = Relation { domain = fromList [1, 2, 3, 4, 5, 6], relation = fromList [(1, 3), (2, 3), (1, 4), (2, 4), (5, 6)] }
graph56xgraph12xGraph34    = Relation { domain = fromList [1, 2, 3, 4, 5, 6], relation = fromList [(1, 3), (2, 3), (1, 4), (2, 4),
                                                                                                   (5, 1), (5, 2), (5, 3), (5, 4), (5, 6),
                                                                                                   (6, 1), (6, 2), (6, 3), (6, 4)] }
-- recznie wklepac w ghci:
-- Connect (pure (*5)) (Union (pure (*10)) (pure (+3)) <*> example34
applicativeResult = "\"edges [(4,5),(5,4),(5,5),(5,6),(5,7),(5,8),(5,10),(5,20),(5,30),(5,40),(5,50),(5,170),(6,8),(7,8),(10,4),(10,5),(10,6),(10,7),(10,8),(10,10),(10,15),(10,20),(10,30),(10,40),(10,50),(10,170),(15,4),(15,5),(15,6),(15,7),(15,8),(15,10),(15,20),(15,25),(15,30),(15,40),(15,50),(15,170),(20,4),(20,5),(20,6),(20,7),(20,8),(20,10),(20,20),(20,25),(20,30),(20,40),(20,50),(20,170),(25,4),(25,5),(25,6),(25,7),(25,8),(25,10),(25,20),(25,30),(25,40),(25,50),(25,170),(30,50),(40,50),(85,4),(85,5),(85,6),(85,7),(85,8),(85,10),(85,20),(85,30),(85,40),(85,50),(85,170)] + vertices []\""


tests :: [Bool]
tests = [
    show graph11                                     == "edges [(1,1)] + vertices []",
    show graph12                                     == "edges [] + vertices [1,2]",
    show graph56                                     == "edges [(5,6)] + vertices []",

    graph12                                          /= graph56,
    graph56                                          == graph56,

    (fromBasic $ graph56 + graph12 * graph34)        == graph56plusgraph12xGraph34,
    (fromBasic $ graph56 * graph12 * graph34)        == graph56xgraph12xGraph34,

    show example34                                   == "edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]",
    todot example34                                  == "digraph {1 -> 2; 2 -> 3; 2 -> 4; 3 -> 5; 4 -> 5; 17}",
    (show $ (+10) <$> example34)                     == "edges [(11,12),(12,13),(12,14),(13,15),(14,15)] + vertices [27]",
    (show $ mergeV 3 4 34 example34)                 == "edges [(1,2),(2,34),(34,5)] + vertices [17]",
    (show $ splitV 34 3 4 (mergeV 3 4 34 example34)) == "edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]",
    (show $ mergeV 5 6 56 graph56)                   == "edges [(56,56)] + vertices []",
    (show $ splitV 56 5 6 $ mergeV 5 6 56 graph56)   == "edges [(5,5),(5,6),(6,5),(6,6)] + vertices []",
    (show $ mergeV 2 3 23 graph1234)                 == "edges [(1,23),(23,4),(23,23)] + vertices []"
  ]


testsStr :: [String]
testsStr = [
    "show graph11                                     == edges [(1,1)] + vertices []",
    "show graph12                                     == edges [] + vertices [1,2]",
    "show graph56                                     == edges [(5,6)] + vertices []",

    "graph12                                          /= graph56",
    "graph56                                          == graph56",

    "(fromBasic $ graph56 + graph12 * graph34)        == graph56plusgraph12xGraph34",
    "(fromBasic $ graph56 * graph12 * graph34)        == graph56xgraph12xGraph34",

    "show example34                                   == edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]",
    "todot example34                                  == digraph {1 -> 2;2 -> 3;2 -> 4;3 -> 5;4 -> 5;17}",
    "(show $ (+10) <$> example34)                     == edges [(11,12),(12,13),(12,14),(13,15),(14,15)] + vertices [27]",
    "(show $ mergeV 3 4 34 example34)                 == edges [(1,2),(2,34),(34,5)] + vertices [17]",
    "(show $ splitV 34 3 4 (mergeV 3 4 34 example34)) == edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]",
    "(show $ mergeV 5 6 56 graph56)                   == edges [(56,56)] + vertices []",
    "(show $ splitV 56 5 6 $ mergeV 5 6 56 graph56)   == edges [(5,5),(5,6),(6,5),(6,6)] + vertices []",
    "(show $ mergeV 2 3 23 graph1234)                 == edges [(1,23),(23,4),(23,23)] + vertices []"
  ]


testsWrong :: [String]
testsWrong = map snd $ filter (not . fst) $ zip tests testsStr

main :: IO ()
main = if length testsWrong > 0 then 
  putStrLn "The following tests failed:" >> mapM_ putStrLn testsWrong 
  else putStrLn "OK"
