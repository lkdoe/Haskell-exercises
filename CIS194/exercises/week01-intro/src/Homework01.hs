module Homework01 where

-- Den Fall, in dem n > 0 ist, als erstes zu prüfen, wäre wahrscheinlich effizienter.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (mod n 10) : toDigitsRev (div n 10)

-- Es wäre effizienter, "reverse" von "toDigitsRev" auszugeben.
toDigits :: Integer -> [Integer]
toDigits n
    | n < 1 = []
    | otherwise = toDigits (div n 10) ++ [mod n 10]

-- Verdopple jede zweite Zahl einer Liste, beginnend von rechts
{-
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther l@(x : y : zs)
    | (length l) `mod` 2 == 0   = (2*x):y:doubleEveryOther zs
    | otherwise                 = x:(2*y):doubleEveryOther zs
-}

{- doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (go (reverse xs))
  where
    go (x:y:zs) = x: (2*y) : go zs
    go xs       = xs -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . go . reverse
  where
    go (x : y : zs) = x : (2 * y) : go zs
    go xs = xs

-- Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n : ns)
    | n < 10 = n + sumDigits ns
    | otherwise = (div n 10) + (mod n 10) + sumDigits ns

validate :: Integer -> Bool
validate n
    | n > 0 = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0
    | otherwise = False

-- Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)

-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n p1 p2 pt -- wobei n Scheiben von p1 nach p2 bewegt werden sollen und pt zum temporären Auslagern benutzt wird.
    | n <= 0 = []
    --    | n == 1        = [(p1,p2)]
    --    | n == 2        = [(p1, pt), (p1,p2), (pt,p2)]
    --    | n == 3        = [(p1, p2), (p1,pt), (p2,pt), (p1,p2), (pt,p1), (pt,p2), (p1,p2)]
    | otherwise = (hanoi (n - 1) p1 pt p2) ++ [(p1, p2)] ++ (hanoi (n - 1) pt p2 p1)

-- Das löst zwar das Problem, mit anderen Worten: die Scheiben sind am Ende in der richtigen Reihenfolge auf p2, aber die Lösung ist nicht optimal.
-- Sie benutzt weit mehr als die minimale Anzahl an Schritten.
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n p1 p2 t1 t2
    | n <= 0 = []
    | n == 1 = [(p1, p2)]
    --    | n == 2        = [(p1,t1),(p1,p2),(t1,p2)]
    | n == 3 = [(p1, t1), (p1, t2), (p1, p2), (t2, p2), (t1, p2)]
    | otherwise = (hanoi4 (n - 1) p1 t1 t2 p2) ++ [(p1, p2)] ++ (hanoi4 (n - 1) t1 p2 t2 p1)

hanoiCombined :: Integer -> Integer -> Peg -> Peg -> Peg -> Peg -> [(Integer, Int)]
hanoiCombined n k p1 p2 t1 t2
    | k >= n = [(k, length $ hanoi4 n p1 p2 t1 t2)]
    | k < 0 = (k, -1) : hanoiCombined 1 k p1 p2 t1 t2
    | otherwise = (k, length (hanoi4 k p1 t2 p2 t1 ++ hanoi (n - k) p1 p2 t1 ++ hanoi4 k t2 p2 p1 t1)) : hanoiCombined n (k + 1) p1 p2 t1 t2
