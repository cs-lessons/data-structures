module Data.BST.Internal where

-- | An AVL Tree
data Tree a
    = Empty                        -- ^ Empty tree
    | Node Int a (Tree a) (Tree a) -- ^ A node with height, label, and left and right children
    deriving (Show, Eq, Ord)

-- | The height of the tree
height :: Tree a -> Int
height Empty          = -1
height (Node h _ _ _) = h

-- | The balance factor of the AVL tree, which is the difference of the heights of its children
balanceFactor :: Tree a -> Int
balanceFactor Empty          = 0
balanceFactor (Node _ _ l r) = height r - height l

-- | Whether or not the tree is empty
nullTree :: Tree a -> Bool
nullTree Empty = True
nullTree _     = False

-- | Creates an empty tree
empty :: Tree a
empty = Empty

-- | Creates a tree of only one element
singleton :: a -> Tree a
singleton x = Node 0 x Empty Empty

-- | Whether an element is in the tree
member :: Ord a => a -> Tree a -> Bool
member _ Empty = False
member x (Node _ y l r) = if
    | x == y -> True
    | x < y  -> member x l
    | x > y  -> member x r

-- | Whether an element is not in the tree
notMember :: Ord a => a -> Tree a -> Bool
notMember x xs = not (member x xs)

-- | Counts the occurrences of an element
count :: Ord a => a -> Tree a -> Int
count _ Empty = 0
count x (Node _ y l r) = if
    | x == y -> count x r + 1
    | x < y  -> count x l
    | x > y  -> count x r

-- | The minimum value of the tree
minValue :: Ord a => Tree a -> Maybe a
minValue Empty = Nothing
minValue n = let Node _ x _ _ = minNode n in Just x

-- | The maximum value of the tree
maxValue :: Ord a => Tree a -> Maybe a
maxValue Empty = Nothing
maxValue n = let Node _ x _ _ = maxNode n in Just x

-- | Inserts a new element
insert :: Ord a => a -> Tree a -> Tree a
insert x Empty = Node 0 x Empty Empty
insert x (Node _ y l r) = if
    | x < y     -> newNode y (insert x l) r
    | otherwise -> newNode y l (insert x r)

-- | Deletes a single occurrence of an element
delete :: Ord a => a -> Tree a -> Tree a
delete x Empty = Empty
delete x (Node _ y l r) = if
    | x < y -> newNode y (delete x l) r
    | x > y -> newNode y l (delete x r)
    | nullTree l && nullTree r -> Empty
    | nullTree l -> r
    | nullTree r -> l
    | otherwise ->
        let (Node _ mx _ _) = minNode r
        in newNode mx l (delete mx r)

-- | The node containing the minimum element
minNode :: Tree a -> Tree a
minNode Empty = error "empty tree"
minNode n@(Node _ _ l _) = if
    | nullTree l -> n
    | otherwise -> minNode l

-- | The node containing the maximum element
maxNode :: Tree a -> Tree a
maxNode Empty = error "empty tree"
maxNode n@(Node _ _ _ r) = if
    | nullTree r -> n
    | otherwise -> maxNode r

-- | Creates a new node with children
newNode :: a -> Tree a -> Tree a -> Tree a
newNode x l r = Node (incHeight l r) x l r

-- | Gets the correct height based on children
incHeight :: Tree a -> Tree a -> Int
incHeight l r = max (height l) (height r) + 1

-- | Converts to list in ascending order
toAscList :: Tree a -> [a]
toAscList = foldrTree (:) []

-- | Converts to list in descending order
toDescList :: Tree a -> [a]
toDescList = foldlTree (flip (:)) []

-- | Converts from list
fromList :: Ord a => [a] -> Tree a
fromList = foldr insert empty

-- | Maps every element
mapTree :: Ord b => (a -> b) -> Tree a -> Tree b
mapTree f = fromList . map f . toAscList

-- | Maps every element, assuming that the mapping function preserves inequalities
mapMonotonic :: Ord b => (a -> b) -> Tree a -> Tree b
mapMonotonic _ Empty = Empty
mapMonotonic f (Node h x l r) = Node h (f x) (mapMonotonic f l) (mapMonotonic f r)

-- | Folds the tree into one value using a left associative operation
foldlTree :: (b -> a -> b) -> b -> Tree a -> b
foldlTree _ z Empty = z
foldlTree f z (Node _ x l r) = foldlTree f (f (foldlTree f z l) x) r

-- | Folds the tree into one value using a right associative operation
foldrTree :: (a -> b -> b) -> b -> Tree a -> b
foldrTree _ z Empty = z
foldrTree f z (Node _ x l r) = foldrTree f (f x (foldrTree f z r)) l

-- | The catamorphism for tree
cataTree :: b -> (a -> b -> b -> b) -> Tree a -> b
cataTree leaf _ Empty = leaf
cataTree leaf node (Node _ x l r) = node x (cataTree leaf node l) (cataTree leaf node r)

-- | Draws the tree
showTree :: Show a => Tree a -> String
showTree = go 0 "T: "
    where
        go d label Empty = replicate (2 * d) ' ' <> label <> "-"
        go d label (Node _ x l r) =
                replicate (2 * d) ' ' <> label <> show x
            <> "\n" <> go (d + 1) "L: " l
            <> "\n" <> go (d + 1) "R: " r

-- | Prints the tree
printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . showTree
