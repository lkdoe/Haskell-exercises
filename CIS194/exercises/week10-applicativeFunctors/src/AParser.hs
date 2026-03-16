{- CIS 194 HW 10
   due Monday, 1 April
-}
{-# OPTIONS_GHC -Wall #-}

module AParser where

import Control.Applicative
import Control.Monad (void)
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing -- fail on the empty input
    f (x : xs) -- check if x satisfies the predicate
    -- if so, return x along with the remainder
    -- of the input (that is, xs)
        | p x = Just (x, xs)
        | otherwise = Nothing -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

\*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
\*Parser> runParser (satisfy isUpper) "abc"
Nothing
\*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- # Exercise 1 Functor instance for Parser
first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-- polemic: fmap :: (a -> b) -> Parser a -> Parser b
-- It took me a lot of time to understand the construction of this instance.
-- The key was to understand that Parser has the type (String -> Maybe (a, String)) -> Parser a.
-- This has to act on some input of type String.
-- the `fmap` should therefore act on the `a` inside the Parser.
-- This `a` is contained inside `Maybe (a, String)`.
-- Without the Functor instance, fmap on Parser a would have to be constructed like this:
-- To get the action of some function f on a Parser p, you have to:
-- `fmap (first f) . (runParser p) "someInputString"`
instance Functor Parser where
    fmap f (Parser p) = Parser (fmap (first f) . p)

------------------------------------------------------------
-- # Exercise 2
-- Instanciate Parser as Applicative:
-- class (Functor f) => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
--
-- The Parser p1 will produce a Pair of an intermediate function intermFunc
-- and the rest1 String that was not parsed.
-- This function has to act on the result of p2 on the rest String by fmap.
instance Applicative Parser where
    pure a = Parser (\s -> Just (a, s))
    p1 <*> p2 = Parser p
      where
        p input = case runParser p1 input of
            Nothing -> Nothing
            Just (intermFunc, rest1) -> first intermFunc <$> runParser p2 rest1

------------------------------------------------------------
-- # Exercise 3

-- abParser will parse for two characters a and b and returns `Just ((a,b),rest)` if successful.
abParser :: Parser (Char, Char)
-- abParser = (\a b -> (a, b)) <$> (char 'a') <*> (char 'b')
abParser = (,) <$> char 'a' <*> char 'b'

-- All of the below versions work:
abParser_ :: Parser ()
-- abParser_ = (\_ -> ()) <$> abParser
-- abParser_ = const () <$> abParser
-- abParser_ = () <$ abParser
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> char ' ' <*> posInt

------------------------------------------------------------
-- # Exercise 4

instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser p
      where
        p input = runParser p1 input <|> runParser p2 input

------------------------------------------------------------
-- # Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void (satisfy isUpper) <|> void posInt
