module TestSet where
import Set
import Test.QuickCheck


leftUnit :: Set Int -> Bool
leftUnit a = mempty <> a == a

rightUnit :: Set Int -> Bool
rightUnit a = a <> empty == a

unionCommutative :: Set Int -> Set Int -> Bool
unionCommutative a b = a <> b == b <> a

unionIdempotent :: Set Int -> Set Int -> Bool
unionIdempotent a b = a <> a == a

assoc :: Set Int -> Set Int -> Set Int -> Bool
assoc x y z = (x<>y)<>z == x<>(y<>z)

reasonable :: Int -> Set Int -> Bool
reasonable a s = member a (singleton a <> s)


main = do
       writeln "testing left unit"       
       quickCheck leftUnit
       writeln "testing right unit"
       quickCheck rightUnit
       writeln "idempotent"
       quickCheck unionIdempotent
       writeln "commutative"
       quickCheck unionCommutative
       writeln "assoc"
       quickCheck assoc
       writeln "reasonable"
       quickCheck reasonable


writeln :: String -> IO ()
writeln = putStrLn

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = fromList <$> arbitrary

