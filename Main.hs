import Data.List
import Data.Maybe

------------------------------------------------------------------------------------
-- DATA TYPES
------------------------------------------------------------------------------------

-- B-Tree order alias
type BTreeOrder = Int

-- BTree data class
data BTree a = Null BTreeOrder | Node BTreeOrder [a] [BTree a]

-- show the tree
-- prints the tree in following format: ((child0) value0 (child1) value1 ... valueN (childN+1))
instance (Show a) => Show (BTree a) where    
    show (Node _ values children) = 
        let
            showedValues = map show values
            showedChildren = map show (tail children)
            showedFirstChild = head children
            zippedValuesAndChildren = zip showedValues showedChildren
        in
            "(" ++ show showedFirstChild ++ concatMap (\x -> " " ++ fst x ++ " " ++ snd x) zippedValuesAndChildren ++ ")" 
    show _ = "Null"

-- represents position of the second node towards the selected node
data NodePosition = Before | After deriving (Eq)

-- represents type of shift
data ShiftType = ShiftFromLeft | ShiftFromRight deriving (Eq)

------------------------------------------------------------------------------------
-- FIND 
------------------------------------------------------------------------------------

-- treeFind :: BTree -> value -> isInTree
-- is this value contained in the tree
treeFind :: (Ord a) => BTree a -> a -> Bool 
treeFind (Node _ values children) value
    | value `elem` values = True
    | otherwise = treeFind (children !! getCountOfLowerElementsInSortedList value values) value
treeFind _ _ = False

------------------------------------------------------------------------------------
-- ADD
------------------------------------------------------------------------------------

-- treeAdd :: tree -> value -> treeWithValue
-- add the value into the tree
treeAdd :: (Ord a) => BTree a -> a -> BTree a
treeAdd node value = balanceRoot $ nodeAdd node value

nodeAdd :: (Ord a) => BTree a -> a -> BTree a
nodeAdd this@(Node order values children) value
    | value `elem` values = error "Value is already contained in the tree"
    | allChildrenAreLeaves this = Node order (insertIntoSorted value values) (Null order : children)
    | otherwise = 
        let
            childIndex = getCountOfLowerElementsInSortedList value values
            lower = take childIndex children
            greater = drop (childIndex+1) children
            newChild = nodeAdd (children !! childIndex) value
        in
            balanceNthChildOverflow (Node order values (lower ++ [newChild] ++ greater)) childIndex
nodeAdd (Null o) value = Node o [value] [Null o, Null o]

------------------------------------------------------------------------------------
-- DELETE
------------------------------------------------------------------------------------

-- treeDelete :: tree -> value -> treeWithoutValue
-- delete selected value from the tree
treeDelete :: (Ord a) => BTree a -> a -> BTree a
treeDelete node value = balanceRoot $ nodeDelete node value

nodeDelete :: (Ord a) => BTree a -> a -> BTree a 
nodeDelete this@(Node order values children) value
    | value `elem` values =
        if allChildrenAreLeaves this
        then
            Node order [ x | x <- values, x /= value ] (tail children)
        else 
            let
                valueIndex = fromJust (elemIndex value values)
                childIndex = valueIndex + 1
                new = extractSmallest (children !! childIndex) 
                newValues = replaceAt valueIndex (fst new) values
                newChildren = replaceAt childIndex (snd new) children
            in
                balanceNthChildUnderflow (Node order newValues newChildren) childIndex
    | allChildrenAreLeaves this && value `notElem` values = error "Value is not contained in the tree."
    | otherwise = 
        let
            childIndex = getCountOfLowerElementsInSortedList value values
            newChild = nodeDelete (children !! childIndex) value
        in
            balanceNthChildUnderflow (Node order values (replaceAt childIndex newChild children)) childIndex
nodeDelete _ _ = error ""

------------------------------------------------------------------------------------
-- BALANCE
------------------------------------------------------------------------------------

-- balanceNthChildUnderflow :: tree -> n -> balancedChildTree
-- balance the tree after delete - if the selected child less than N children, borrow node from its neighbour or merge with the neighbour
balanceNthChildUnderflow :: (Ord a) => BTree a -> Int -> BTree a
balanceNthChildUnderflow this@(Node o _ children) n 
    | getChildrenCount nthChild >= minOrder o = this
    | canShiftIntoNthChildFrom this n ShiftFromLeft = shift this n ShiftFromLeft
    | canShiftIntoNthChildFrom this n ShiftFromRight = shift this n ShiftFromRight
    | otherwise = 
        if n > 0
        then
            mergeNthChildWith this n Before
        else
            mergeNthChildWith this n After
    where nthChild = children !! n
balanceNthChildUnderflow tree _ = tree 

-- balanceNthChildOverflow :: tree -> n -> balancedChildTree
-- balance the tree after insert - if the selected child has more than 2N children, split it into 2 and insert the value into this node
balanceNthChildOverflow :: (Ord a) => BTree a -> Int -> BTree a
balanceNthChildOverflow this@(Node order _ children) n
    | getChildrenCount nthChild <= order = this
    | otherwise =
        let
            splitValues = split nthChild
            newValue = fst splitValues
            newChildren = snd splitValues
        in
            addValueAndChildren this newValue newChildren
    where nthChild = children !! n
balanceNthChildOverflow tree _ = tree 

-- balance the root node
balanceRoot :: (Ord a) => BTree a -> BTree a
balanceRoot this@(Node order _ children)
    | cc < 2 = head children
    | cc > order =
        let
            splitValues = split this
            newValue = fst splitValues
            newChildren = snd splitValues
        in
            Node order [newValue] [fst newChildren, snd newChildren]
    | otherwise = this
    where cc = getChildrenCount this
balanceRoot tree = tree

-- mergeNthChildWith :: tree -> n -> with
-- merge n-th child with its left/right sibling
mergeNthChildWith :: (Ord a) => BTree a -> Int -> NodePosition -> BTree a
mergeNthChildWith (Node o values children) n np
    | np == After = 
        let
            right = children !! (n+1)
            v = values !! n
            newChild = merge nthChild v right
            newChildren = replaceAt n newChild $ deleteAt (n+1) children 
        in
            Node o (deleteAt n values) newChildren
    | otherwise =
        let
            left = children !! (n-1)
            v = values !! (n-1)
            newChild = merge left v nthChild
            newChildren = replaceAt (n-1) newChild $ deleteAt n children 
        in
            Node o (deleteAt (n-1) values) newChildren
    where
        nthChild = children !! n
mergeNthChildWith _ _ _ = error "Cannot merge leaf node"

-- merge two BTrees using a separator and return the result 
merge :: (Ord a) => BTree a -> a -> BTree a -> BTree a
merge (Node o1 v1 c1) v (Node o2 v2 c2)
    | o1 == o2 = Node o1 (v1 ++ v:v2) (c1 ++ c2)
    | otherwise = error "Order of trees is different"
merge _ _ _ = error "Cannot merge leaf nodes"

-- shift :: tree -> n -> direction
-- shift values into the n-th child (either from left or right sibling)
-- used in balancing after delete
shift :: (Ord a) => BTree a -> Int -> ShiftType -> BTree a
shift (Node o values children) n shiftType
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
            Node o newValues newChildren
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
            Node o newValues newChildren
    where
        current = children !! n
shift _ _ _ = error "Cannot shift a leaf node"

-- can we shift values into n-th child from its neighbour (shift from left or right)
canShiftIntoNthChildFrom :: (Ord a) => BTree a -> Int -> ShiftType -> Bool
canShiftIntoNthChildFrom this@(Node _ _ children) n shiftType
    | shiftType == ShiftFromLeft =
        n > 0 && not (hasMinimalChildrenCount (children !! (n-1)))
    | otherwise = 
        n < (getChildrenCount this - 1) && not (hasMinimalChildrenCount (children !! (n+1)))
canShiftIntoNthChildFrom _ _ _ = False

-- extract first child and return ((first value,first child), remaining tree)
extractFirstChild :: BTree a -> ((a, BTree a), BTree a)
extractFirstChild (Node o (fv:rv) (fc:rc)) = 
    ((fv,fc),Node o rv rc)
extractFirstChild _ = error "Cannot extract from a leaf node"

-- extract last child and return ((last value,last child), remaining tree)
extractLastChild :: BTree a -> ((a, BTree a), BTree a)
extractLastChild (Node o values children) = 
    ((last values, last children), Node o (init values) (init children))
extractLastChild _ = error "Cannot extract from a leaf node"

-- extract smallest value and return it together with the balanced new tree
extractSmallest :: (Ord a) => BTree a -> (a, BTree a)
extractSmallest this@(Node o values children)
    | allChildrenAreLeaves this =
        (head values, Node o (tail values) (tail children))
    | otherwise = 
        let
            extracted = extractSmallest $ head children
            remaining = Node o values (snd extracted : tail children)
        in
            (fst extracted, balanceNthChildUnderflow remaining 0)
extractSmallest _ = error "Cannot extract" 

-- add value and child into the tree and return it
addValueAndChild :: (Ord a) => (a, BTree a) -> BTree a -> NodePosition -> BTree a
addValueAndChild (value,child) (Node o values children) position =
    let
        valueIndex = getCountOfLowerElementsInSortedList value values
        childIndex = if position == Before then valueIndex else 1 + valueIndex
    in  
        Node o (insertAt valueIndex value values) (insertAt childIndex child children)
addValueAndChild _ _ _ = error ""

-- split :: tree -> (median, (firstHalfTree,secondHalfTree)
-- split the tree - return median and 2 B-trees split by this value
split :: (Ord a) => BTree a -> (a,(BTree a, BTree a))
split (Node o values children) = 
    let 
        median = length values `div` 2
        firstHalfValues = take median values
        secondHalfValues = drop (median+1) values
        firstHalfChildren = take (median+1) children
        secondHalfChildren = drop (median+1) children
    in
        (values !! median,(Node o firstHalfValues firstHalfChildren, Node o secondHalfValues secondHalfChildren))
split _ = error "Cannot split"

-- addValueAndChildren :: tree -> value -> twoChildren -> newTree
-- add value and children to the selected tree (used in balancing after insert)
addValueAndChildren :: (Ord a) => BTree a -> a -> (BTree a, BTree a) -> BTree a
addValueAndChildren (Node o values children) newValue newChildren =
    let
        s = getCountOfLowerElementsInSortedList newValue values
    in
        Node o (take s values ++ [newValue] ++ drop s values) (take s children ++ [fst newChildren,snd newChildren] ++ drop (s+1) children)
addValueAndChildren _ _ _ = error "Cannot add to Leaf"

------------------------------------------------------------------------------------
-- FOLD
------------------------------------------------------------------------------------

-- treeFold :: f -> start -> tree -> result
-- fold the tree
treeFold :: ([b] -> [a] -> b) -> b -> BTree a -> b
treeFold f start (Node _ values children) = f (map (treeFold f start) children) values
treeFold _ start _ = start

------------------------------------------------------------------------------------
-- LIST
------------------------------------------------------------------------------------

-- treeToList :: tree -> list
-- convert the tree into list (values are ordered)
treeToList :: BTree a -> [a]
treeToList (Node _ values children) = treeToList (head children) ++ concat (zipWith (:) values (map treeToList (tail children)))
treeToList _ = []

-- listToTree :: list -> treeOrder -> tree
-- add values from list one by one into the tree
listToTree :: (Ord a) => [a] -> Int -> BTree a
listToTree [] o = Null o
listToTree (x:xs) o = treeAdd (listToTree xs o) x 

------------------------------------------------------------------------------------
-- HELPER FUNCTIONS
------------------------------------------------------------------------------------

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

minOrder :: BTreeOrder -> Int
minOrder o = ceiling ((fromIntegral o :: Double)/ 2) :: Int

-- determine if this node has minimal number of children
hasMinimalChildrenCount :: BTree a -> Bool
hasMinimalChildrenCount this@(Node order _ _) = getChildrenCount this == minOrder order
hasMinimalChildrenCount _ = error "It is a leaf"

-- get children count of this node
getChildrenCount :: BTree a -> Int
getChildrenCount (Node _ _ children) = length children
getChildrenCount _ = 0

-- determines if all children are Null 
-- because B-Tree has all leafs in the same depth, it's sufficient to just check if the first child is Null
allChildrenAreLeaves :: BTree a -> Bool
allChildrenAreLeaves (Node _ _ ((Null _):_)) = True
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
