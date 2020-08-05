data Tree a = Null | Node [a] [Tree a]

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

treeFind :: (Ord a) => a -> Tree a -> Bool 
treeFind value (Node values _) = value `elem` values
treeFind _ _ = False

treeAdd :: (Ord a) => a -> Tree a -> Tree a
treeAdd value Null = Node [value] []
treeAdd value (Node values children) = Node (insertIntoSorted value values) children

-- treeDelete :: (Ord a) => a => Tree a -> Tree a 

treeToList :: Tree a -> [a]
treeToList Null = []
treeToList (Node values children) = treeToList (head children) ++ concat (zipWith (:) values (map treeToList (tail children)))

treeFold :: ([b] -> [a] -> b) -> b -> Tree a -> b
treeFold _ start Null = start
treeFold f start (Node values children) = f (map (treeFold f start) children) values

-- insertIntoSorted value list - insert value into sorted list and return it
insertIntoSorted :: (Ord a) => a -> [a] -> [a]
insertIntoSorted value [] = [value]
insertIntoSorted value (x:xs)
    | value < x = value:x:xs
    | otherwise = x:insertIntoSorted value xs