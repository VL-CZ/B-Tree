-- BTree data class
data BTree a = Null | Node [a] [BTree a] | EmptyTree

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
treeAdd :: (Ord a) => a -> BTree a -> BTree a
treeAdd value EmptyTree = Node [value] [Null,Null]
treeAdd value (Node values children) = Node (insertIntoSorted value values) children

-- -- treeDelete :: value -> tree -> treeWithoutValue
-- treeDelete :: (Ord a) => a => BTree a -> BTree a 

-- rebalanceNthChild :: n -> tree -> balancedChildTree
-- balance the tree after insert - if the selected child has more than 2N items, split it into 2 and insert the value into this node
rebalanceNthChild :: (Ord a) => Int -> BTree a -> BTree a
rebalanceNthChild n this@(Node values children)
    | getChildrenCount nthChild  <= 3 = this
    | otherwise =
        let
            splitValues = split nthChild
            newValue = fst splitValues
            newChildren = snd splitValues
        in
            addValueAndChildren this newValue newChildren
    where nthChild = children !! n
rebalanceNthChild _ tree = tree 

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

-- split :: tree -> (median, (firstHalfTree,secondHalfTree)
-- split the tree - return median and 2 B-trees split by this value
split :: (Ord a) => BTree a -> (a,(BTree a, BTree a))
split (Node values children) = 
    let 
        median = length values `div` 2
        firstHalfValues = take median values
        secondHalfValues = drop (median+1) values
        firstHalfChildren = take (median+1) children
        secondHalfChildren = drop (median+1) children
    in
        (values !! median,(Node firstHalfValues firstHalfChildren,Node secondHalfValues secondHalfChildren))
split _ = error "Cannot split"

-- addValueAndChildren :: tree -> value -> twoChildren -> newTree
-- add value and children to the selected tree (used in balancing after insert)
addValueAndChildren :: (Ord a) => BTree a -> a -> (BTree a, BTree a) -> BTree a
addValueAndChildren (Node values children) newValue newChildren =
    let
        s = getCountOfLowerElementsInSortedList newValue values
    in
        Node (take s values ++ [newValue] ++ drop s values) (take s children ++ [fst newChildren,snd newChildren] ++ drop (s+1) children)
addValueAndChildren _ _ _ = error "Cannot add to Leaf"

-- get children count of this node
getChildrenCount :: BTree a -> Int
getChildrenCount (Node _ children) = length children
getChildrenCount _ = 0