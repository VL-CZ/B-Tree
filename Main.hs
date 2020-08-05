-- B-Tree data class
data Tree a = Null | Node [a] [Tree a]

-- show the tree
-- prints the tree in following format: ((child0) value0 (child1) value1 ... valueN (childN+1))
instance (Show a) => Show (Tree a) where  
    show Null = "(Null)"  
    show (Node values children) = 
        let
            showedValues = map show values
            showedChildren = map show (tail children)
            showedFirstChild = head children
            zippedValuesAndChildren = zip showedValues showedChildren
        in
            "(" ++ show showedFirstChild ++ concatMap (\x -> " " ++ fst x ++ " " ++ snd x) zippedValuesAndChildren ++ ")" 

-- treeFind :: value -> Tree -> isInTree
-- is this value contained in the tree
treeFind :: (Ord a) => a -> Tree a -> Bool 
treeFind _ Null = False
treeFind value (Node values children)
    | value `elem` values = True
    | otherwise = treeFind value (children !! getCountOfLowerElementsInSortedList value values)

-- treeAdd :: value -> tree -> treeWithValue
-- add the value into the tree
treeAdd :: (Ord a) => a -> Tree a -> Tree a
treeAdd value Null = Node [value] []
treeAdd value (Node values children) = Node (insertIntoSorted value values) children

-- -- treeDelete :: value -> tree -> treeWithoutValue
-- treeDelete :: (Ord a) => a => Tree a -> Tree a 

-- treeToList :: tree -> list
-- convert tree into list (values are ordered)
treeToList :: Tree a -> [a]
treeToList Null = []
treeToList (Node values children) = treeToList (head children) ++ concat (zipWith (:) values (map treeToList (tail children)))

-- treeFold :: f -> start -> tree -> result
-- fold the tree
treeFold :: ([b] -> [a] -> b) -> b -> Tree a -> b
treeFold _ start Null = start
treeFold f start (Node values children) = f (map (treeFold f start) children) values

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