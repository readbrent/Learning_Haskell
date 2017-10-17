module Fibonacci where

-- Basic Fibonacci Function
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Infinite Fibonacci List
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- "Efficient"" Fibonacci
fibs2 :: [Integer]
fibs2 = [0, 1] ++ (append 0 1) where
    append :: Integer -> Integer -> [Integer]
    append x_2 x_1 = sum : (append x_1 sum) where
        sum = x_2 + x_1

-- Using zipWith magic
fibs2' :: [Integer]
fibs2' = 0 : 1 : zipWith (+) fibs2' (tail fibs2')

-- Exercise 3: Streams!
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show s = "Stream " ++ (show $ take 20 $ streamToList s)