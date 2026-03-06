{-# OPTIONS_GHC -Wall #-}

module Lecture where

-- import Control.Applicative

type Name = String

data Employee = Employee
    { name :: Name
    , phone :: String
    }
    deriving (Show)

fmap2 :: (Functor f) => (a -> b -> c) -> (f a -> f b -> f c)
fmap2 = undefined

ex01 :: Maybe Integer
ex01 = Just (+ 3) <*> Just 9

ex02 :: Maybe Integer
-- ex02 = pure (+) <*> Just 3 <*> Just 5
ex02 = (+) <$> Just 3 <*> Just 5

-- Example of a data type that can be made into a pathological instance of Functor.
-- It is not a functor in the mathematical sense.
-- Functor definition:
{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}
-- Functor rules:
-- fmap id = id                       -- or fmap id (F a) = id (F a) = F a
-- fmap (f . g) = fmap f . fmap g     -- or fmap (f . g ) (F a) = fmap f (fmap g (F a))

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap _ CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- This violates the first functor rule.
