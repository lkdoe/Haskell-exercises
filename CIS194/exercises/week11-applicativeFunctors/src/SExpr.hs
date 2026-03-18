{-# OPTIONS_GHC -Wall #-}

{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- This is the function `many :: Applicative f => f a -> f [a]`
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- This is the function `some :: Applicative f => f a -> f [a]`
-- Equivalent:
-- oneOrMore p = liftA2 (:) p (zeroOrMore p)
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
    deriving (Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr
    = A Atom
    | Comb [SExpr]
    deriving (Show)

parseAtom :: Parser Atom
-- parseAtom = (\i -> N i) <$> posInt <|> (\s -> I s) <$> ident
parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
-- parseSExpr = char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')' <|> A <$> parseAtom
parseSExpr =
    spaces
        *> (A <$> parseAtom <|> char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')')
        <* spaces
