import Data.List
import System.IO


insertlist :: a -> Int -> [a] -> [a]
insertlist element 0 x = element : x
insertlist element n (x:xs) = x : insertlist element (n-1) xs

perm :: a -> [a] -> Int -> [[a]]
perm e x (-1) = []
perm e x n = insertlist e n x : perm e x (n-1)

-- 2.1 (a)
distribute :: a -> [a] -> [[a]]
distribute element [] = [[]]
distribute element x = perm element x (sum[1 | _ <- x])

rec1 :: a -> [a] -> [[a]]
rec1 x (y:ys) = [x : m | m <- distribute y ys]


rec2 :: Int -> [a] -> [[[a]]]
rec2 0 x = []
rec2 l (x:xs) = concat[rec1 x xs : rec2 (l-1) (xs ++ [x])]

-- 2.1 (b)
permutation :: [a] -> [[a]]
permutation x = concat (rec2 (sum[1 | _ <- x]) x)

---------------------------------------------------------

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

single :: a -> Tree a  
single x = Node x Empty Empty  

-- 2.2 (a)
ourTree :: Tree Int
ourTree = Node 51 (Node 10 Empty Empty) (Node 17 (Node 8 Empty Empty) (Node 42 Empty Empty))

-- 2.2 (b)
treeRow :: Int -> Tree a -> [a]
treeRow (Node a left right) 0 = [a]
treeRow n Empty = []
treeRow n (Node a left right) = treeRow (n-1) left ++ treeRow (n-1) right

treeHeight :: Tree a -> Int
treeHeight Empty = 1
treeHeight (Node a left right) = 1 + if treeHeight right > treeHeight left then treeHeight right else treeHeight left

rec4 :: Tree a -> Int -> [a]
rec4 t n | n > treeHeight t = []
rec4 t n = treeRow t n ++ rec4 t (n+1)

-- 2.2 (c)
breadthFirst :: Tree a -> [a]
breadthFirst t = rec4 t 0

--------------------------------------------------------




