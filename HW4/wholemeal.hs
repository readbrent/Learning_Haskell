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
treeHeight Leaf = -1
treeHeight (Node height _ _ _) = height

xor :: [Bool] -> Bool
xor = foldr xor' False

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- Generates all potential values and then removes the invalid ones.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let deletionList = invalidList n in
    2:[2*x + 1 | x <- [1..n], not (x `elem` deletionList)]

-- Generate the numbers that should not be included.
invalidList :: Integer -> [Integer]
invalidList n = [i + j + 2*i*j | i <- [1..n], j <- [i..n]]

-- Sample list comprehension.
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]