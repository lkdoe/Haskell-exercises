module Calc where

import ExprT
import Parser (parseExp)

-- # Exercise 1
eval :: ExprT -> Integer
eval expr = case expr of
    (Lit i) -> i
    (Add e1 e2) -> eval e1 + eval e2
    (Mul e1 e2) -> eval e1 * eval e2

-- # Exercise 2
