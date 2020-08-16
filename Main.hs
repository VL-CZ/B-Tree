-- BTree data class
data BTree a = Null | Node [a] [BTree a] | Top

-- show the tree
-- prints the tree in following format: ((child0) value0 (child1) value1 ... valueN (childN+1))
instance (Show a) => Show (BTree a) where    
    show (Node values children) = 
        let
            showedValues = map show values
            showedChildren = map show (tail children)
            showedFirstChild = head children
            zippedValuesAndChildren = zip showedValues showedChildren
        in
            "(" ++ show showedFirstChild ++ concatMap (\x -> " " ++ fst x ++ " " ++ snd x) zippedValuesAndChildren ++ ")" 
    show _ = "(Null)"

-- treeFind :: value -> BTree -> isInTree
-- is this value contained in the tree
treeFind :: (Ord a) => a -> BTree a -> Bool 
treeFind value (Node values children)
    | value `elem` values = True
    | otherwise = treeFind value (children !! getCountOfLowerElementsInSortedList value values)
treeFind _ _ = False

-- treeAdd :: value -> tree -> treeWithValue
-- add the value into the tree
-- treeAdd :: (Ord a) => a -> BTree a -> BTree a

-- treeAdd value (Null _) = Node [value] []
-- treeAdd value (Node values children _) = Node (insertIntoSorted value values) children

-- -- treeDelete :: value -> tree -> treeWithoutValue
-- treeDelete :: (Ord a) => a => BTree a -> BTree a 

-- treeToList :: tree -> list
-- convert tree into list (values are ordered)
treeToList :: BTree a -> [a]
treeToList (Node values children) = treeToList (head children) ++ concat (zipWith (:) values (map treeToList (tail children)))
treeToList _ = []

-- treeFold :: f -> start -> tree -> result
-- fold the tree
treeFold :: ([b] -> [a] -> b) -> b -> BTree a -> b
treeFold f start (Node values children) = f (map (treeFold f start) children) values
treeFold _ start _ = start

-- insertIntoSorted :: value -> list -> result 
-- insert value into sorted list and return it
insertIntoSorted :: (Ord a) => a -> [a] -> [a]
insertIntoSorted value [] = [value]
insertIntoSorted value (x:xs)
    | value < x = value:x:xs
    | otherwise = x:insertIntoSorted value xs

-- get number of lower elements than value in ordered list
getCountOfLowerElementsInSortedList :: (Ord a) => a -> [a] -> Int
getCountOfLowerElementsInSortedList _ [] = 0
getCountOfLowerElementsInSortedList value (first:rest)
    | value < first = 0
    | otherwise = 1 + getCountOfLowerElementsInSortedList value rest