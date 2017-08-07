-- Peg number
type Peg = String

-- Move from peg [0] to peg [1]
type Move = (Peg, Peg)

-- Returns a list of moves
-- Assumes that all N discs are stacked on the first peg.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 
    let
        step1Moves = hanoi (n - 1) a c b
        step2Move = (a, b)
        step3Move = hanoi (n - 1) c b a
    in
        step1Moves ++ [step2Move] ++ step3Move  

main = do
    -- [("a","c"), ("a","b"), ("c","b")]
    print (hanoi 2 "a" "b" "c") 