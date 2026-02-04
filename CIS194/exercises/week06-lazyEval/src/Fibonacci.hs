module Fibonacci where

-- # Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 = [fib n | n <- [1 ..]]

--------------------------------------------------
-- # Exercise 2
-- This is a very popular solution that can be found all over the internet.
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--------------------------------------------------
-- # Exercise 3
-- Stream should be a type that defines a necessarily infinite list.
-- The difference between a Stream and a list is that it has no constructor for the empty case.
--

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

instance (Show a) => Show (Stream a) where
    show s = show (take 20 $ streamToList s) ++ "..."

printStream :: (Show a) => Int -> Stream a -> String
printStream i s = show (take i $ streamToList s) ++ " (first " ++ show i ++ " elements of Stream)"

-- Not part of the exercise, but I wanted to try it out.
instance Functor Stream where
    fmap f (Cons a as) = Cons (f a) (fmap f as)

--------------------------------------------------
-- # Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f s = f <$> s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

--------------------------------------------------
-- # Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
-- interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- For a given natural number n > 0,
-- the ruler function returns the highest power of 2 that divides n.
-- If defined as a Stream, ruler can be used to get the result of
-- ruler([1..] == 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,. . .)
-- https://oeis.org/A001511 -- Eric D. Burgess:
-- 1 interleaved (2 interleaved (...))
ruler :: Stream Integer
ruler = iterateInterleave 0
  where
    iterateInterleave j = interleaveStreams (streamRepeat j) (iterateInterleave (j + 1))

-- Alternative rom ChatGPT:
-- ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
-- Not required for this exercise but I used it for brainstorming
rule :: Integer -> Integer
rule i
    | i < 1 = 0
    | even i = 1 + rule (div i 2)
    | otherwise = 0

--------------------------------------------------
-- # Exercise 6
