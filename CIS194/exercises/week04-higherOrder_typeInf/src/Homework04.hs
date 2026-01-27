module Homework04 where

import Data.Char (toUpper)

-- # Exercise 1: Wholemeal programming
-- 1. fun1 :: [Integer]-> Integer
--    fun1 [] = 1
--    fun1 (x:xs)
--      | even x = (x - 2) * fun1 xs
--      | otherwise = fun1 xs
--
-- 2. fun2 :: Integer-> Integer
--    fun2 1 = 0
--    fun2 n | even n = n + fun2 (n ‘div‘ 2)
--           | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
    sum
        . filter even
        . takeWhile (/= 1)
        . iterate (\x -> if even x then div x 2 else 3 * x + 1)

-- # Exercise 2: Folding with trees
-- A node of the tree contains an Integer that represents the height of the subtree at this node.
-- Counting starts at 0.

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
-- foldTree [] = Leaf
-- foldTree [x] = Node 0 Leaf x Leaf
-- foldTree (x : xs) = insertNode x (foldTree xs)
foldTree = foldr insertNode Leaf

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node _ Leaf _ Leaf) = 0
treeHeight (Node _ l _ r) = 1 + max (treeHeight l) (treeHeight r)

-- Positive balance denotes a tree that has greater hight on its right side.
-- Negative balance means left side has greater hight.
-- I didn't use it in the solution, though I tested my trees for balance when brainstorming solutions.
treeBalance :: Tree a -> Integer
treeBalance Leaf = 0
treeBalance (Node _ l _ r) = treeHeight r - treeHeight l

insertNode :: a -> Tree a -> Tree a
insertNode a Leaf = Node 0 Leaf a Leaf
insertNode a old@(Node h l n r)
    | treeHeight l < treeHeight r = Node h' (insertNode a l) n r
    | otherwise = Node h' l n (insertNode a r)
  where
    h' = treeHeight (insertNode a old)
