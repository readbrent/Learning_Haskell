module Hopscotch where

skips :: [a] -> [[a]]
skips [] = []
skips xs = skipWithCounter 1 xs
    where
        skipWithCounter :: Int -> [a] -> [[a]]
        skipWithCounter n xs 
            | n > length xs = []
            | otherwise = everyNth n xs : skipWithCounter (n+1) xs

everyNth :: Int -> [a] -> [a]
everyNth 0 _ = []
everyNth 1 xs = xs
everyNth n xs = everyNthWithIndex 1 n xs
    where
        everyNthWithIndex :: Int -> Int -> [a] -> [a]
        everyNthWithIndex _ _ [] = []
        everyNthWithIndex index n (x:xs)
        -- If we're on index N then concatenate the current value/ 
        -- Otherwise just increase the index.
            | (index == n) = [x] ++ everyNthWithIndex 1 n xs
            | otherwise = everyNthWithIndex (index + 1) n xs
