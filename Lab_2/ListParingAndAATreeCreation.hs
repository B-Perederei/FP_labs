import Data.List (intercalate)

-- First assigment
pairL  [a] - [[a]]
pairL []                       = []
pairL list  length list  2   = [list]
pairL list                     = (head list  [list!!1])  pairL (drop 2 list)

-- Second assigment
data AATree a = Empty  Node a Int (AATree a) (AATree a)

instance Show a = Show (AATree a) where
    show x = intercalate n (draw x)

showNode x level = [intercalate  [show x, tt(, show level, )]]

draw  Show a = AATree a - [String] 
draw Empty = [nil]
draw (Node x level Empty Empty) = showNode x level
draw (Node x level left right)  = showNode x level ++ partright (draw right) ++ partleft (draw left)
    where partleft  = zipWith (++) ( repeat  )
          partright = zipWith (++) ( repeat  )

leaf   a - AATree a
leaf x = Node x 1 Empty Empty

skew  AATree a - AATree a
skew (Node a level (Node left levelL leftL rightL) right)
         levelL == level = Node left levelL leftL (Node a level rightL right)
skew x = x

split  AATree a - AATree a
split (Node a level left (Node right levelR leftR rightright@(Node _ levelRR _ _)))
         levelRR == level = Node right (levelR+1) (Node a level left leftR) rightright
split x = x

insert  (Ord a) = a - AATree a - AATree a
insert x Empty = leaf x
insert x (Node a level left right)
         x == a = Node a level left right
         x  a  = split(skew(Node a level (insert x left) right))
         x  a  = split(skew(Node a level left (insert x right)))
         otherwise = error Can't match data types

-- MAIN
main = do
    -- First assigment tests
    putStrLn -FIRST TASK-
    print ['A'..'G']
    print $pairL ['A'..'G']
    print [1..8]
    print $pairL [1..8]

    -- Second assigment tests
    putStrLn n-SECOND TASK-
    putStrLn Elements
    print [1..16]
    putStrLn AA-Tree (Second number in Node is level)
    print $foldr insert Empty (reverse [1..16]) -- Inserting elements in order from 1 to 16
