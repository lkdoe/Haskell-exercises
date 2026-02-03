{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import ExprT
import Parser (parseExp)
import StackVM

-- # Exercise 1
eval :: ExprT -> Integer
eval expr = case expr of
    (ExprT.Lit i) -> i
    (ExprT.Add e1 e2) -> eval e1 + eval e2
    (ExprT.Mul e1 e2) -> eval e1 * eval e2

-- # Exercise 2
-- evalStr :: String-> Maybe Integer
-- Neat thing: Learned a bit about functors and fmap as well as its infix (<$>).
-- First I was confused why parseExp Lit Add Mul by itself got a type error asking for Integers
-- (as type a in the definition of Lit, Add and Mul).
-- Then I made the connection to the signature of evalStr and it clicked.

evalStr :: String -> Maybe Integer
evalStr exp = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul exp

-- # Exercise 3

class Expr e where
    lit :: Integer -> e
    add :: e -> e -> e
    mul :: e -> e -> e

-- "ExprT." is optional if only the Expr module is imported.
-- It is necessary when StackVM is imported also,
-- because both modules have Add and Mul as constructors.
instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

-- Given as an example for constraining the type of arguments:
reify :: ExprT -> ExprT
reify = id

-- For Exercise 5:
programify :: Program -> Program
programify = id

-- # Exercise 4
-- The definition of the infix operators could also be done in this way:
-- add = (+)
instance Expr Integer where
    lit = id
    add n1 n2 = n1 + n2 -- add = (+)
    mul n1 n2 = n1 * n2 -- mul = (*)

instance Expr Bool where
    lit n = n > 0
    add n1 n2 = n1 || n2 -- add = (||)
    mul n1 n2 = n1 && n2 -- mul = (&&)

newtype MinMax = MinMax Integer
    deriving (Show, Eq, Ord)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 n1) (Mod7 n2) = lit (n1 + n2)
    mul (Mod7 n1) (Mod7 n2) = lit (n1 * n2)

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 *-4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- # Exercise 5
-- Use the simulated VM from the provided StackVM.hs.
-- Create an instance of Expr for the type StackVM.Program.
-- Then create a compile function that creates a Program from a String.
instance Expr StackVM.Program where
    lit n = [StackVM.PushI n]
    add n1 n2 = n1 ++ n2 ++ [StackVM.Add]
    mul n1 n2 = n1 ++ n2 ++ [StackVM.Mul]

-- To avoid name collisions with the constructors of ExprT:
-- Unfortunately, this turned out not to work when compiling the StackVM.Program.
-- However, when directly passing the output to the stackVM for execution,
-- it worked fine. The qualified name of the constructor simply is left out when printing it.
litStack :: Integer -> Program
litStack n = [StackVM.PushI n]
addStack :: Program -> Program -> Program
addStack n1 n2 = n1 ++ n2 ++ [StackVM.Add]
mulStack :: Program -> Program -> Program
mulStack n1 n2 = n1 ++ n2 ++ [StackVM.Mul]

-- Reminder:
-- evalStr :: String -> Maybe Integer
-- evalStr exp = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul exp

compile :: String -> Maybe StackVM.Program
-- compile ex1 = programify <$> parseExp litStack addStack mulStack ex1
compile = parseExp litStack addStack mulStack

-- # Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT
    = VLit Integer
    | VAdd VarExprT VarExprT
    | VMul VarExprT VarExprT
    | VVar String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul
instance HasVars VarExprT where
    var = VVar

-- This takes a variable name s and returns a function from a Map to Maybe Integer.
-- The function takes a Map and returns from it Just the value for the given name or Nothing.
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit i = \_ -> Just i
    add e1 e2 = \m -> case (e1 m, e2 m) of
        (Just i, Just j) -> Just (i + j)
        _ -> Nothing
    mul e1 e2 = \m -> case (e1 m, e2 m) of
        (Just i, Just j) -> Just (i * j)
        _ -> Nothing

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

dict = M.fromList [("a", 1), ("b", 2)]
