import qualified TestSet
import qualified TestBasicGraph
import qualified TestRelation


main = do
  writeln "Set"
  TestSet.main
  writeln "Basic"
  TestBasicGraph.main
  writeln "Relation"
  TestRelation.main

writeln :: String -> IO ()
writeln = putStrLn
