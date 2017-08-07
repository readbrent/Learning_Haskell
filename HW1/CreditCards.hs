-- Converts a number to digits
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
  | n <= 0 = []
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

-- Converts a number to digits and then reverses it
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Doubles every other value
doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x : y : zs) =  x : 2 * y : doubleEveryOther zs 

doubleEveryOtherFromEnd :: [Integer] -> [Integer]
doubleEveryOtherFromEnd = reverse . doubleEveryOther

-- Sums the digits of the credit card.
sumDigits :: [Integer] -> Integer

sumDigits [] = 0
sumDigits (x:xs)
  | x < 10 = x + sumDigits xs
  | otherwise = (x `mod` 10) + (x `div` 10) + sumDigits xs

--- Inidicates whether the credit card is valid.
validate :: Integer -> Bool
validate x =  (sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10) == 0

-- Janky Tests!
main = do 
        print (toDigits 1234) -- [1,2,3,4]
        print (toDigitsRev 1234) -- [4,3,2,1]
        print (doubleEveryOtherFromEnd [8,7,6,5]) -- [16,7,12,5]
        print (doubleEveryOtherFromEnd [1,2,3]) -- [1,4,3]
        print (sumDigits [16,7,12,5]) -- 22
        print (validate 4012888888881881) -- True
        print (validate 4012888888881882) -- False 