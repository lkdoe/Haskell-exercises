module Lecture where

import Data.Array

-- Roughly what Data.List(foldl') does; can just be imported.
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x : xs) =
    let z' = z `f` x
     in seq z' $ foldl' f z' xs

-- Custom boolean AND that does not short-circuit.
-- Haskell's And is defined along the lines of (True && x) -> x; (False && _) -> False
(&&!) :: Bool -> Bool -> Bool
True &&! True = True
True &&! False = False
False &&! True = False
False &&! False = False

knapsack01 ::
    [Double] -> -- values
    [Integer] -> -- nonnegative weights
    Integer -> -- knapsack size
    Double -- max possible value
knapsack01 vs ws maxW = m ! (numItems - 1, maxW)
  where
    numItems = length vs
    m =
        array ((-1, 0), (numItems - 1, maxW)) $
            [((-1, w), 0) | w <- [0 .. maxW]]
                ++ [((i, 0), 0) | i <- [0 .. numItems - 1]]
                ++ [ ((i, w), best)
                   | i <- [0 .. numItems - 1]
                   , w <- [1 .. maxW]
                   , let best
                            | ws !! i > w = m ! (i - 1, w)
                            | otherwise =
                                max
                                    (m ! (i - 1, w))
                                    (m ! (i - 1, w - ws !! i) + vs !! i)
                   ]
