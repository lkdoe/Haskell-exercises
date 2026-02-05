{-# OPTIONS_GHC-fno-warn-missing-methods #-}
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
-- "optional but very cool" Fibonacci numbers via generating functions
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate = fmap (* (-1))
    (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
    (*) (Cons a as) bb@(Cons b bs) = Cons (a * b) (fmap (* a) bs + as * bb)

-- Important: To keep all the entries as Integer, use the div function.
-- (/) would turn the fraction of Intergers a and b (a/b) into a Float.
instance Fractional (Stream Integer) where
    (/) aa@(Cons a as) bb@(Cons b bs) = Cons (div a b) ((`div` b) <$> (as - ((aa / bb) * bs)))

-- If F(x) = F_0 + F_1*x + F_2*x^2 + ... with Fn being the n-th Fibonacci number,
-- then F_(n+2) == F_n + F_(n+1) and thus F = 0 + 1*x + F*x + F*X^2.
-- It follows that x = F - xF - (x^2)F, or F = x/(1-x-x^2)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x ^ 2))

--------------------------------------------------
-- # Exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
        Matrix
            (a11 * b11 + a12 * b21)
            (a11 * b12 + a12 * b22)
            (a21 * b11 + a22 * b21)
            (a21 * b12 + a22 * b22)

fBase = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = go (fBase ^ n)
  where
    go (Matrix _ a _ _) = a
