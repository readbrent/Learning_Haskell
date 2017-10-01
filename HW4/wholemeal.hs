module Wholemeal where

-- Original
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even 


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
    where
        insert :: a -> Tree a -> Tree a
        insert x Leaf = Node 0 Leaf x Leaf
        insert x (Node height left val right) 
            | treeHeight left <= treeHeight right =
                let new_left = (insert x left)
                in Node (treeHeight new_left + 1) new_left val right 
            | otherwise = 
                let new_right = (insert x right)
                in Node (treeHeight new_right + 1) left val new_right

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node height _ _ _) = height