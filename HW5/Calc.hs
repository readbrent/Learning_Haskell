module Calc where
import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = parseExp Lit Add Mul s >>= (\x -> Just (eval x))

-- Exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT.ExprT where
    lit n = ExprT.Lit n
    add a b = ExprT.Add a b
    mul a b = ExprT.Mul a b

-- Exercise 4
instance Expr Integer where
    lit a = a
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit n = n > 0
    add a b = a || b
    mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit n = MinMax n
    add (MinMax a) (MinMax b) = MinMax $ min a b
    mul (MinMax a) (MinMax b) = MinMax $ max a b

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
    mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

-- Tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
