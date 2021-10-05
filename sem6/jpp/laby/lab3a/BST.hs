module BST (insert, remove, fromList, toList, BST(Empty)) where


data BST a = Empty
           | Node (BST a) a (BST a)

-- Napisać własne instancje Show i Eq

instance Show a => Show (BST a) where
  show Empty                 = "null"
  show (Node left val right) = show (left, val, right)


instance Eq a => Eq (BST a) where
  Empty == Empty = True
  (Node left1 val1 right1) == (Node left2 val2 right2) = and [
      left1 == left2,
      val1 == val2,
      right1 == right2
    ]
  _ == _ = False


-- Wierzchołki w kolejności in-order
-- Żeby było efektywniej można użyć DList,
-- która zapewnia konkatenację w czasie stałym.
-- (Za to konwersja na zwykłą listę oraz oblicznie
-- pierwszego elementu zajmują czas liniowy).
toList :: BST a -> [a]
toList Empty                 = []
toList (Node left val right) = toList left ++ [val] ++ toList right


insert :: (Ord a) => BST a -> a -> BST a
insert Empty                         newVal = Node Empty newVal Empty
insert (Node leftTree val rightTree) newVal
  | newVal > val                            = Node leftTree val (insert rightTree newVal)
  | newVal < val                            = Node (insert leftTree newVal) val rightTree
  | otherwise                               = Node leftTree val rightTree


fromList :: (Ord a) => [a] -> BST a
fromList = foldl insert Empty


member :: (Ord a) => BST a -> a -> Bool
member Empty                         _             = False
member (Node leftTree val rightTree) searchedValue
  | searchedValue > val                            = member rightTree searchedValue
  | searchedValue < val                            = member leftTree searchedValue
  | otherwise                                      = True


remove :: (Ord a) => BST a -> a -> BST a
remove Empty                         _            = Empty
remove (Node leftTree val rightTree) removedValue
  | removedValue > val                            = Node leftTree val (remove rightTree removedValue)
  | removedValue < val                            = Node (remove leftTree removedValue) val rightTree
  | otherwise                                     = removeRoot $ (Node leftTree val rightTree)

removeRoot :: (Ord a) => BST a -> BST a
removeRoot Empty                         = error "removing root from an empty tree"
removeRoot (Node Empty    _   rightTree) = rightTree
removeRoot (Node leftTree _ rightTree)   = case removeMax leftTree of
  (tree, max) -> (Node tree max rightTree)

removeMax :: (Ord a) => BST a -> (BST a, a)
removeMax Empty                         = error "removing max from an empty tree"
removeMax (Node Empty    val Empty)     = (Empty, val)
removeMax (Node leftTree val Empty)     = (leftTree, val)
removeMax (Node leftTree val rightTree) = case removeMax rightTree of
  (tree, max) -> (Node leftTree val tree, max)



-- Na koniec tworzymy moduł Sorting z funkcją sort, która
-- sortuje poprzez BST
