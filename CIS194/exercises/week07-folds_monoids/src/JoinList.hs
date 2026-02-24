{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE TypeSynonymInstances #-}

module JoinList where

import Buffer
import Control.Applicative ((<|>))
import qualified Data.Monoid as M
import Editor
import Scrabble
import Sized

-- Example:
-- data JoinListBasic a
--     = Empty
--     | Single a
--     | Append (JoinListBasic a) (JoinListBasic a)
--
-- jlbToList :: JoinListBasic a -> [a]
-- jlbToList Empty = []
-- jlbToList (Single a) = [a]
-- jlbToList (Append l1 l2) = jlbToList l1 ++ jlbToList l2

-- m is considered a monoidal annotation, that is, its values derive from some monoid.
-- The annotation of a root node will be the combination of all of its child nodes
-- according to the combination action of the monoid.

data JoinList m a
    = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

ex01 :: JoinList Size String
ex01 = Append 6 (Single 1 "Eins") (Append 5 (Single 2 "Zwei") (Single 3 "Drei"))

ex02 :: JoinList Size Integer
ex02 =
    Append
        (Size 4)
        ( Append
            (Size 3)
            ( Append
                (Size 2)
                (Single (Size 1) 1)
                (Single (Size 1) 2)
            )
            (Single (Size 1) 3)
        )
        (Single (Size 1) 4)

ex03 =
    fromString "This\nThat\nAnd the other\nAlso consider\nFoo\n\bar\nBaz\n" ::
        JoinList (Score, Size) String

------------------------------------------------------------
-- # Exercise 1
--
-- Helper function to get the annotation at the root of a JoinList:
tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Write an append function for JoinList:
(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

------------------------------------------------------------
-- # Exercise 2

-- Example function for converting JoinList to list:
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Example function for retreiving the i-th element from a list.
-- The behaviour of indexJ should be:
-- (indexJ i jl) == (jlToList jl !!? i)
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

-- The following does not solve the assignment as intended.
-- Instead it searches a JoinList for a specific key index for arbitrary Sized indices.
-- It does not consider what the JL would look like if converted to a list.
-- The only conditions are that child nodes have lower indices than their parent,
-- and no index appears more than once (more occurances will not be found).
indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' _ Empty = Nothing
indexJ' i (Single b a)
    | getSize (size b) == i = Just a
    | otherwise = Nothing
indexJ' i (Append b jl1 jl2)
    | getSize (size b) >= i = indexJ' i jl1 <|> indexJ' i jl2
    | otherwise = Nothing

-- Another helper function that gets the Size of a JoinListL
tagSize :: (Sized b, Monoid b) => JoinList b a -> Int
tagSize jlst = getSize $ size $ tag jlst

-- This version only works on a JoinList if it is of a form as if constructed like this:
-- (Single 1 a1) +++ (Single 1 a2) +++ ...
-- Each Single must have a Size 1.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append j jl1 jl2)
    | i < 0 = Nothing
    | i > getSize (size j) = Nothing
    | otherwise = indexJ i jl1 <|> indexJ (i - tagSize jl1) jl2
--   where
--     tagSize jlst = getSize $ size $ tag jlst
indexJ _ _ = Nothing

-- Intended behaviour: jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl
    | i < 1 = jl
dropJ i (Append j jl1 jl2)
    | i >= s = Empty
    | i < tagSize jl1 = dropJ i jl1 +++ jl2
    | i >= tagSize jl1 = dropJ (i - tagSize jl1) jl2
  where
    s = getSize . size $ j
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl
    | i >= tagSize jl = jl
    | i < 1 = Empty
takeJ i (Append j jl1 jl2)
    | i < tagSize jl1 = takeJ i jl1
    | otherwise = jl1 +++ takeJ (i - tagSize jl1) jl2

------------------------------------------------------------
-- # Exercise 3: Scrabble
-- See Scrabble.hs

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine s = Single (scoreString s) s

------------------------------------------------------------
-- # Exercise 4
instance Buffer (JoinList (Score, Size) String) where
    --     toString jl = case jl of
    --         Empty -> ""
    --         (Single _ st) -> st
    --         (Append _ j1 j2) -> toString j1 ++ toString j2
    toString = unlines . jlToList

    --  Below is my own solution, but it produces an unbalanced JoinList,
    --  and due to the `unlines`, runs in O(n^2)
    --     fromString txt =
    --         let lns = lines txt
    --          in case lns of
    --                 [] -> Empty
    --                 [l] -> Single (scoreString l, 1) l
    --                 (l : ls) -> Single (scoreString l, 1) l +++ (fromString . unlines $ ls)
    -- With help of ChatGPT:
    fromString = build . map mkSingle . lines
      where
        mkSingle s = Single (scoreString s, 1) s
        build [] = Empty
        build [x] = x
        build xs =
            let (l, r) = splitAt (length xs `div` 2) xs
             in build l +++ build r
    line = indexJ
    replaceLine i _ jl
        | i < 0 || i >= tagSize jl = jl
    replaceLine 0 s (Single _ _) = Single (scoreString s, 1) s
    replaceLine i s (Append _ l r)
        | i < tagSize l = replaceLine i s l +++ r
        | otherwise = l +++ replaceLine (i - tagSize l) s r
    numLines = tagSize
    value (Append (Score v, _) _ _) = v
    value (Single (Score v, _) _) = v
    value _ = 0

-- Main function
--

main = runEditor editor (fromString "Start Editor" :: JoinList (Score, Size) String)
