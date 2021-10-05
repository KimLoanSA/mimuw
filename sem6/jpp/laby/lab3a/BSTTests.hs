import           BST

a = Empty
b = fromList [4, 3, 2, 5, 7, 12, 1, 34]

not $ a == b

b == b

toList b == [1,2,3,4,5,7,12,34]

toList (remove b 5) == [1,2,3,4,7,12,34]

toList (remove b 7) == [1,2,3,4,5,12,34]

toList (remove b 4) == [1,2,3,5,7,12,34]

toList (remove b 12) == [1,2,3,4,5,7,34]

c = remove b 7

toList (insert c 8 ) == [1,2,3,4,5,8,12,34]
