module Golf where

-- Not actually a code-golf version, but rather experimentation for solving the problems themselves first. Might add shorter versions later.

-- # Exercise 1: Hopscotch
-- skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1]          == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips []           == []

-- Helper function for the "skips" function, see below
takeEveryNth :: Int -> [a] -> [a]
takeEveryNth _ [] = []
takeEveryNth n lst =
    let reducedL = drop (n - 1) lst
     in case reducedL of
            [] -> []
            (x : xs) -> x : takeEveryNth n xs

-- Shorter version from ChatGPT that zips each element with its index.
-- Then returns list of exactly those elements that have index divisible by the target number.
--
-- takeEveryNth n xs = [x | (x,i) <- zip xs [1..], i `mod` n == 0]
--
-- It is possible to write both functions in one. Also shorter names would reduce the character count.

skips :: [a] -> [[a]]
skips [] = []
skips lst = [takeEveryNth n lst | n <- [1 .. (length lst)]]

-- # Exercise 2 Local maxima
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

localMaxima :: [Integer] -> [Integer]
localMaxima lst = case lst of
    (a : b : c : d) ->
        if b > a && b > c
            then b : localMaxima (b : c : d)
            else localMaxima (b : c : d)
    _ -> []

-- Shorter version, again from ChatGPT:
-- localMaxima :: [Integer] -> [Integer]
-- localMaxima xs =
--   [b | (a,b,c) <- zip3 xs (drop 1 xs) (drop 2 xs), b > a && b > c]

-- # Exercise 3 Histogram
--
-- This solution is far from optimal.
-- Counting and constructing the individual lines can be done more efficiently through list comprehension.

histogram :: [Integer] -> String
histogram intlst = (concat . reverse . buildHist . countNumbers $ intlst) ++ "\n==========\n0123456789\n"

countNumbers :: [Integer] -> [Integer]
countNumbers [] = replicate 10 0
countNumbers (x : xs) = zipWith (+) (replicate n 0 ++ [1] ++ replicate (9 - n) 0) (countNumbers xs)
  where
    n = fromInteger x

decLst :: [Integer] -> [Integer]
decLst = map (\x -> if x > 0 then x - 1 else x)

buildHist :: [Integer] -> [String]
buildHist [] = [""]
buildHist [0, 0, 0, 0, 0, 0, 0, 0, 0, 0] = [""]
buildHist xs = map (\n -> if n > 0 then '*' else ' ') xs : "\n" : buildHist (decLst xs)

{-
More efficient solution by Bernhard Schwarzenbacher,
https://github.com/bschwb/cis194-solutions/blob/main/03-rec-poly/Golf.hs:

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

-- returns one * line from the above function
-- line :: [Int] -> Int -> String
-- line xs n = [if i >= n then '*' else ' ' | i <- xs]
--
-- -- counts occurence of numbers in [0..9] in the input list.
-- count :: [Integer] -> [Int]
-- count xs = map (\n -> length $ filter (== n) xs) [0..9]
-}
