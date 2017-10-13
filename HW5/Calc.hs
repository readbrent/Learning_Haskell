module Calc where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = (eval a) + (eval b)
eval (ExprT.Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr s = parseExp Lit Add Mul s >>= (\x -> Just (eval x))