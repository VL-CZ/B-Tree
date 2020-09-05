import Data.List
import Data.Maybe

-- BTree data class
data BTree a = Null | Node [a] [BTree a]

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
    show _ = "Null"

data NodePosition = Before | After deriving (Eq)
data ShiftType = ShiftFromLeft | ShiftFromRight deriving (Eq)

-- treeFind :: BTree -> value -> isInTree
-- is this value contained in the tree
treeFind :: (Ord a) => BTree a -> a -> Bool 
treeFind (Node values children) value
    | value `elem` values = True
    | otherwise = treeFind (children !! getCountOfLowerElementsInSortedList value values) value
treeFind _ _ = False

-- treeAdd :: tree -> value -> treeWithValue
-- add the value into the tree
treeAdd :: (Ord a) => BTree a -> a -> BTree a
treeAdd node value = rebalanceRoot $ nodeAdd node value

nodeAdd :: (Ord a) => BTree a -> a -> BTree a
nodeAdd this@(Node values children) value
    | value `elem` values = this 
    | allChildrenAreLeaves this = Node (insertIntoSorted value values) (Null:children)
    | otherwise = 
        let
            childIndex = getCountOfLowerElementsInSortedList value values
            lower = take childIndex children
            greater = drop (childIndex+1) children
            newChild = nodeAdd (children !! childIndex) value
        in
            rebalanceNthChild (Node values (lower ++ [newChild] ++ greater)) childIndex
nodeAdd Null value = Node [value] [Null,Null]

-- treeDelete :: tree -> value -> treeWithoutValue
-- delete selected value from tree
treeDelete :: (Ord a) => BTree a -> a -> BTree a 
treeDelete this@(Node values children) value
    | value `elem` values =
        if allChildrenAreLeaves this
        then
            Node [ x | x <- values, x /= value ] (tail children)
        else 
            let
                valueIndex = fromJust (elemIndex value values)
                childIndex = valueIndex + 1
                new = extractSmallest (children !! childIndex) 
                newValues = replaceAt valueIndex (fst new) values
                newChildren = replaceAt childIndex (snd new) children
            in
                Node newValues newChildren
    | allChildrenAreLeaves this && value `notElem` values = error "Value is not in the tree."
    | otherwise = 
        let
            childIndex = getCountOfLowerElementsInSortedList value values
            newChild = treeDelete (children !! childIndex) value
        in
            Node values (replaceAt childIndex newChild children)
treeDelete _ _ = error ""

-- shift :: tree -> n -> direction
-- shift values into the n-th child (either from left or right sibling)
-- used in balancing after delete
shift :: (Ord a) => BTree a -> Int -> ShiftType -> BTree a
shift (Node values children) n shiftType
    | shiftType == ShiftFromRight = 
        let
            oldValue = values !! n
            right = children !! (n+1)
            extracted = extractFirstChild right
            newRight = snd extracted
            extractedValues = fst extracted
            newCurrent = addValueAndChild (oldValue, snd extractedValues) current After
            newValues = replaceAt n (fst extractedValues) values
            newChildren = replaceAt n newCurrent $ replaceAt (n+1) newRight children
        in
            Node newValues newChildren
    | otherwise = 
        let
        oldValue = values !! (n-1)
        left = children !! (n-1)
        extracted = extractLastChild left
        newLeft = snd extracted
        extractedValues = fst extracted
        newCurrent = addValueAndChild (oldValue, snd extractedValues) current Before
        newValues = replaceAt (n-1) (fst extractedValues) values
        newChildren = replaceAt n newCurrent $ replaceAt (n-1) newLeft children
        in
            Node newValues newChildren
    where
        current = children !! n
shift _ _ _ = error "Cannot shift a leaf node"

-- can we shift values into n-th child from its neighbour (shift from left or right)
canShiftIntoNthChildFrom :: (Ord a) => BTree a -> Int -> ShiftType -> Bool
canShiftIntoNthChildFrom this@(Node _ children) n shiftType
    | shiftType == ShiftFromLeft =
        n > 0 && not (hasMinimalChildrenCount (children !! (n-1)))
    | otherwise = 
        n < (getChildrenCount this - 1) && not (hasMinimalChildrenCount (children !! (n+1)))
canShiftIntoNthChildFrom _ _ _ = False

-- extract first child and return ((first value,first child), remaining tree)
extractFirstChild :: BTree a -> ((a, BTree a), BTree a)
extractFirstChild (Node (fv:rv) (fc:rc)) = 
    ((fv,fc),Node rv rc)
extractFirstChild _ = error "Cannot extract from a leaf node"

-- extract last child and return ((last value,last child), remaining tree)
extractLastChild :: BTree a -> ((a, BTree a), BTree a)
extractLastChild (Node values children) = 
    ((last values, last children),Node (init values) (init children))
extractLastChild _ = error "Cannot extract from a leaf node"

-- add value and child into the tree and return it
addValueAndChild :: (Ord a) => (a, BTree a) -> BTree a -> NodePosition -> BTree a
addValueAndChild (value,child) (Node values children) position =
    let
        valueIndex = getCountOfLowerElementsInSortedList value values
        childIndex = if position == Before then valueIndex else 1 + valueIndex
    in  
        Node (insertAt valueIndex value values) (insertAt childIndex child children)
addValueAndChild _ _ _ = error ""

-- extract smallest value and return it together with the new tree
extractSmallest :: BTree a -> (a, BTree a)
extractSmallest this@(Node values children)
    | allChildrenAreLeaves this =
        (head values, Node (tail values) (tail children))
    | otherwise = 
        let
            firstChild = extractSmallest $ head children
        in
            (fst firstChild, Node values (snd firstChild : tail children))
extractSmallest _ = error "Cannot extract" 

-- rebalanceNthChild :: tree -> n -> balancedChildTree
-- balance the tree after insert - if the selected child has more than 2N items, split it into 2 and insert the value into this node
rebalanceNthChild :: (Ord a) => BTree a -> Int -> BTree a
rebalanceNthChild this@(Node _ children) n
    | getChildrenCount nthChild  <= 3 = this
    | otherwise =
        let
            splitValues = split nthChild
            newValue = fst splitValues
            newChildren = snd splitValues
        in
            addValueAndChildren this newValue newChildren
    where nthChild = children !! n
rebalanceNthChild tree _ = tree 

-- rebalance root
rebalanceRoot :: (Ord a) => BTree a -> BTree a
rebalanceRoot this@(Node _ _)
    | getChildrenCount this <= 3 = this
    | otherwise =
        let
            splitValues = split this
            newValue = fst splitValues
            newChildren = snd splitValues
        in
            Node [newValue] [fst newChildren, snd newChildren]
rebalanceRoot tree = tree

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

hasMinimalChildrenCount :: BTree a -> Bool
hasMinimalChildrenCount this@(Node _ _) = getChildrenCount this == 2
hasMinimalChildrenCount _ = error "It is a leaf"

-- get children count of this node
getChildrenCount :: BTree a -> Int
getChildrenCount (Node _ children) = length children
getChildrenCount _ = 0

-- determines if all children are Null 
-- because B-Tree has all leafs in the same depth, it's sufficient to just check if the first child is Null
allChildrenAreLeaves :: BTree a -> Bool
allChildrenAreLeaves (Node _ (Null:_)) = True
allChildrenAreLeaves _ = False

-- insert element into list at the n-th position and return the new list
insertAt :: Int -> a -> [a] -> [a]
insertAt n value list = take n list ++ [value] ++ drop n list

-- insert the element at n-th position and return the new list
deleteAt :: Int -> [a] -> [a]
deleteAt n list = take n list ++ drop (n+1) list

-- replace the element at n-th position with newValue and return the list
replaceAt :: Int -> a -> [a] -> [a]
replaceAt n newValue list = take n list ++ (newValue : drop (n+1) list)
