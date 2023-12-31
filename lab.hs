import Data.List (delete)

isittrue x = if x
    then "Yes"
    else "No"

isitreallytrue x = if x then "Really yes" else "Really no"

primes = sieve [2..]
    where
        sieve (p:xs) = p : sieve [x | x <- xs, mod x p > 0]

primefactors n = factor n primes
    where
        factor n (p:ps)
            | n < 2         = []
            | p * p > n     = [n]
            | mod n p == 0  = p : factor (div n p) (p:ps)
            | otherwise     = factor n ps

unionOfTwoLists :: Eq a => [a] -> [a] -> [a]
unionOfTwoLists xss@(x:xs) ys
    | xss == ys     = xss
    | null ys       = xss
    | null xss      = ys
    | null xs       = if x `elem` ys then ys else x : ys
    | x `elem` ys   = x : unionOfTwoLists xs (delete x ys)
    | otherwise     = x : unionOfTwoLists xs ys