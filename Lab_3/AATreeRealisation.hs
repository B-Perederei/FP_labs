import Data.List (intercalate)
data AATree a = Empty | Node a Int (AATree a) (AATree a) deriving (Eq)

leaf  :: a -> AATree a
leaf x = Node x 1 Empty Empty

getLvl :: AATree a -> Int
getLvl Empty = 0
getLvl (Node _ level _ _) = level

getRight :: AATree a -> AATree a
getRight Empty = error "Node is Empty"
getRight (Node _ _ _ right) = right

getLeft :: AATree a -> AATree a
getLeft Empty = error "Node is Empty"
getLeft (Node _ _ left _) = left

xpred :: AATree a -> a
xpred Empty = error "Node is Empty"
xpred (Node a _ _ Empty) = a
xpred (Node a _ _ right) = xpred right

xsucc :: AATree a -> a
xsucc Empty = error "Node is Empty"
xsucc (Node a _ Empty _) = a
xsucc (Node a _ left _) = xsucc left

instance Show a => Show (AATree a) where
    show x = intercalate "\n" (draw x)
showNode x level = [intercalate "" [show x, "\t\t(", show level, ")"]]
draw :: Show a => AATree a -> [String] 
draw Empty = ["nil"]
draw (Node x level Empty Empty) = showNode x level
draw (Node x level left right)  = showNode x level ++ partright (draw right) ++ partleft (draw left)
    where partleft  = zipWith (++) ("|": repeat " ")
          partright = zipWith (++) ("|": repeat "| ")

skew :: AATree a -> AATree a
skew (Node a level (Node left levelL leftL rightL) right)
        | levelL == level = Node left levelL leftL (Node a level rightL right)
skew x = x

split :: AATree a -> AATree a
split (Node a level left (Node right levelR leftR rightright@(Node _ levelRR _ _)))
        | levelRR == level = Node right (levelR+1) (Node a level left leftR) rightright
split x = x

isInTree :: (Ord a) => a -> AATree a -> Bool
isInTree x Empty = False
isInTree x (Node a _ left right)
        | x == a = True
        | x < a  = isInTree x left
        | x > a  = isInTree x right
isInTree x _ = error "Data type is corrupted"

decreaseLvl :: AATree a -> AATree a
decreaseLvl x@(Node a level Empty Empty) = x
decreaseLvl (Node a level left Empty)
        | 1 < level  = Node a 1 left Empty
decreaseLvl (Node a level left (Node right levelR leftR rightR))
        | expected < level && expected < levelR = Node a expected left (Node right expected leftR rightR)
        | expected < level                      = Node a expected left (Node right levelR leftR rightR)
        where expected = minimum (getLvl left : [levelR]) + 1
decreaseLvl x = x

balanceDel :: (Ord a) => AATree a -> AATree a
balanceDel x
        | sd == Empty          = Empty
        | getRight sd == Empty = let Node a level left right             = split sd in Node a level left (split right)
        | otherwise            = let Node a level left right             = sd
                                     Node aR levelR leftR rightR         = skew right
                                     Node aout levelout leftout rightout = split $ Node a level left (Node aR levelR leftR (skew rightR))
                                     in Node aout levelout leftout (split rightout)
        where sd = skew (decreaseLvl x)

delete :: (Ord a) => a -> AATree a -> AATree a
delete x Empty = Empty
delete x (Node a level left right)
        | x < a                                     = balanceDel (Node a level (delete x left) right)
        | x > a                                     = balanceDel (Node a level left (delete x right))
        | x == a && right == Empty && left == Empty = Empty
        | x == a && right == Empty && left /= Empty = balanceDel (Node (xpred left) level (delete (xpred left) left) Empty)
        | x == a && right /= Empty                  = balanceDel (Node (xsucc right) level left (delete (xsucc right) right))
        | otherwise                                 = Node a level left right

insert :: (Ord a) => a -> AATree a -> AATree a
insert x Empty = leaf x
insert x (Node a level left right)
        | x == a = Node a level left right
        | x < a  = split(skew(Node a level (insert x left) right))
        | x > a  = split(skew(Node a level left (insert x right)))
        | otherwise = error "Can't match data types"

task :: (Ord a) => a -> AATree a -> AATree a
task x t
    | isInTree x t = delete x t
    | otherwise    = insert x t

-- MAIN
main = do
    putStrLn "Initial AA-Tree (Second number in Node is level)"
    let aatree = foldr insert Empty (reverse [1..18])
    let nodeToDelete = 8
    let nodeToInsert = 25
    print aatree
    putStrLn $ "\nExample of deleting node " ++ show nodeToDelete
    print $ task nodeToDelete aatree
    putStrLn $ "\nExample of inserting node " ++ show nodeToInsert
    print $ task nodeToInsert aatree