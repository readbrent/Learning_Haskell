import qualified Data.Map as M
 
type Count = Integer
type Counts = M.Map Integer Count

type Indices = [Integer]

histogram :: [Integer] -> String
histogram xs = hitogram' . toCounts
    where histogram'
        histogram' :: Counts -> String
        hitogram' =
            let (max_list, xs') = trim xs
            in if null max_list
                    then footer
                    else asterisks max_list ++ histogram xs'


trim :: Counts -> ([Integer], Counts)
trim = undefined

asterisks :: Indices -> String
asterisks = undefined

toCounts :: [Integer] -> Counts
toCounts = foldr insertOrAdjust M.empty

insertOrAdjust :: Integer -> Counts -> Counts
insertOrAdjust n cs
    | M.member n cs = M.adjust +1 n cs
    | otherwise = M.insert n 1 cs