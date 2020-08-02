data Tree a = Null | Node a [Tree a] deriving(Show)

-- treeFind :: (Ord a) => a -> Tree a -> Bool 

treeAdd :: (Ord a) => a -> Tree a -> Tree a
treeAdd toAdd Null = Node toAdd []
treeAdd toAdd (Node value children) = 
    let newNode = Node toAdd []
    in Node value (newNode:children)

-- treeDelete :: (Ord a) => a => Tree a -> Tree a 

-- treeToList :: Tree a -> [a]

treeFold :: ([b] -> a -> b) -> b -> Tree a -> b
treeFold _ start Null = start
treeFold f start (Node value children) = f (map (treeFold f start) children) value