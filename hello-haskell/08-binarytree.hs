-- Binary Search Tree enforcing uniqueness in elements
-- not balanced!
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

-- in order
toList :: Tree a -> [a]
toList EmptyTree = []
toList (Node a left right) = toList left ++ [a] ++ toList right

-- pre order
toListPre :: Tree a -> [a]
toListPre EmptyTree = []
toListPre (Node a left right) = [a] ++ toListPre left ++ toListPre right

-- post order
toListPost :: Tree a -> [a]
toListPost EmptyTree = []
toListPost (Node a left right) = toListPost right ++ toListPost left ++ [a]

