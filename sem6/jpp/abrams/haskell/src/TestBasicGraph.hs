-- {-# LANGUAGE FlexibleInstances #-}
module TestBasicGraph where
import Graph
import Test.QuickCheck
import Test.QuickCheck.Function

leftUnit :: Basic Int -> Bool
leftUnit a = empty * a == a

rightUnit :: Basic Int -> Bool
rightUnit a = a * empty == a

connectAssociative :: Basic Int -> Basic Int -> Basic Int -> Bool
connectAssociative x y z = (x * y) * z == x * (y * z)

unionCommutative :: Basic Int -> Basic Int -> Bool
unionCommutative a b = a + b == b + a

unionAssociative :: Basic Int -> Basic Int -> Basic Int -> Bool
unionAssociative x y z = (x + y) + z == x + (y + z)

unionIdempotent :: Basic Int -> Basic Int -> Bool
unionIdempotent a b = a + a == a

distributive :: Basic Int -> Basic Int -> Basic Int -> Bool
distributive x y z = x*(y+z) == x*y + x*z

decomposable :: Basic Int -> Basic Int -> Basic Int -> Bool
decomposable x y z = x*y*z == x*y + x*z + y*z

monadRightId :: Basic Int -> Bool
monadRightId x = (x >>= return) == x

monadLeftId :: Int -> (Fun Int (Basic Int)) -> Bool
monadLeftId x (Fun _ g) = (return x >>= g) == g x

monadAssocProp :: Basic Int -> (Fun Int (Basic Int)) -> (Fun Int (Basic Int)) -> Bool
monadAssocProp x (Fun _ f) (Fun _ g) = ((x >>= f) >>= g) == (x >>= (\x' -> f x' >>= g))

monadAssocProp' :: Basic Int -> Basic Int -> Basic Int -> Bool
monadAssocProp' x y z = ((x >> y) >> z) == (x >> (y >> z))


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
       writeln "idempotent"
       quickCheck unionIdempotent
       writeln "distributive"
       quickCheck distributive
       writeln "decomposable"
       quickCheck decomposable
       writeln "monad"
       quickCheck monadRightId
       quickCheck monadLeftId
       writeln "monadAssocProp'"
       quickCheckWith stdArgs monadAssocProp'
       writeln "monadAssocProp"
       quickCheckWith stdArgs monadAssocProp

writeln :: String -> IO ()
writeln = putStrLn

-- instance (Arbitrary a, Graph g) => Arbitrary (g a) where
instance (Arbitrary a) => Arbitrary (Basic a) where
  arbitrary = sized arb where
    arb 0 = return empty
    arb 1 = vertex <$> arbitrary
    arb n = oneof [ union <$> arb2 <*> arb2
                  , connect <$> arb' <*> arb'] where
      arb2 = arb (div n 4)
      arb' = arb (intSqrt n)
      intSqrt :: Int -> Int
      intSqrt = round . sqrt . fromIntegral
