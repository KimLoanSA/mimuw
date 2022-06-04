module TestRelation where
import Graph
import Test.QuickCheck
import TestSet()

leftUnit :: Relation Int -> Bool
leftUnit a = empty * a == a

rightUnit :: Relation Int -> Bool
rightUnit a = a * empty == a

connectAssociative :: Relation Int -> Relation Int -> Relation Int -> Bool
connectAssociative x y z = (x * y) * z == x * (y * z)

unionCommutative :: Relation Int -> Relation Int -> Bool
unionCommutative a b = a + b == b + a

unionAssociative :: Relation Int -> Relation Int -> Relation Int -> Bool
unionAssociative x y z = (x + y) + z == x + (y + z)

distributive :: Relation Int -> Relation Int -> Relation Int -> Bool
distributive x y z = x*(y+z) == x*y + x*z

decomposable :: Relation Int -> Relation Int -> Relation Int -> Bool
decomposable x y z = x*y*z == x*y + x*z + y*z

main = do
       writeln "connect left unit"       
       quickCheck leftUnit
       writeln "connect right unit"
       quickCheck rightUnit
       writeln "connect associative"
       quickCheck connectAssociative
       writeln "commutative"
       quickCheck unionCommutative
       writeln "union associative"
       quickCheck unionAssociative
       writeln "distributive"
       quickCheck distributive
       writeln "decomposable"
       quickCheck decomposable
       
writeln :: String -> IO ()
writeln = putStrLn

instance (Ord a, Arbitrary a) => Arbitrary (Relation a) where
  arbitrary = sized arb where
    arb 0 = return empty
    arb 1 = vertex <$> arbitrary
    arb n = oneof [ union <$> arb2 <*> arb2
                  , connect <$> arb' <*> arb'] where
      arb2 = arb (div n 2)
      arb' = arb (intSqrt n)
      intSqrt :: Int -> Int
      intSqrt = round . sqrt . fromIntegral

  shrink (Relation d r) =[Relation d r' | r' <- shrink r]
