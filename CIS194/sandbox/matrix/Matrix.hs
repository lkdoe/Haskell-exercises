module Matrix where

import qualified Data.Array as Array

newtype Matrix = Matrix (Array.Array (Int, Int) Integer)

mkMatrix :: Int -> Int -> [Integer] -> Maybe Matrix
mkMatrix n m elems =
    if n >= 0 && m >= 0 && length elems == (n + 1) * (m + 1)
        then Just (Matrix (Array.listArray ((0, 0), (n, m)) elems))
        else Nothing

mkDiag :: Int -> Matrix
mkDiag n
    | n <= 1 = Matrix (Array.array ((0, 0), (0, 0)) [((0, 0), 1)])
    | otherwise = Matrix (Array.array ((0, 0), (n - 1, n - 1)) (diagArray n))
  where
    diagArray n = [((i, j), (\i j -> if i == j then 1 else 0) i j) | i <- [0 .. (n - 1)], j <- [0 .. (n - 1)]]

------------------------------------------------------------
-- Example
ex01 = mkMatrix 1 1 [1, 2, 3, 4]

ex02 = Matrix (Array.listArray ((0, 0), (2, 2)) [1, 2, 3, 4, 5, 6, 7, 8, 9])

------------------------------------------------------------
mElems :: Matrix -> [Integer]
mElems (Matrix arr) = Array.elems arr

mBounds :: Matrix -> ((Int, Int), (Int, Int))
mBounds (Matrix arr) = Array.bounds arr

mAssocs :: Matrix -> [((Int, Int), Integer)]
mAssocs (Matrix arr) = Array.assocs arr

instance Show Matrix where
    show m =
        let asso = mAssocs m in display asso
      where
        display asso = case asso of
            [] -> "\n"
            [((_, _), i)] -> show i ++ "\n"
            (((r1, _), i) : rest@(((r2, _), _) : as))
                | r1 == r2 -> show i ++ " " ++ display rest
                | r1 /= r2 -> show i ++ "\n" ++ display rest
            _ -> "Too much\n"

-- Matrix product
(.*.) :: Matrix -> Matrix -> Maybe Matrix
a .*. b = mProduct a b

row :: Matrix -> Int -> [Integer]
row (Matrix m) i = [(Array.!) m (i, j) | j <- [(snd $ fst $ Array.bounds m) .. (snd $ snd $ Array.bounds m)]]

col :: Matrix -> Int -> [Integer]
col (Matrix m) j = [(Array.!) m (i, j) | i <- [(fst $ fst $ Array.bounds m) .. (fst $ snd $ Array.bounds m)]]

mProduct :: Matrix -> Matrix -> Maybe Matrix
mProduct a b =
    let
        bounds = (mBounds a, mBounds b)
        assocA = mAssocs a
        assocB = mAssocs b
     in
        case bounds of
            (((rowA0, colA0), (rowAn, colAn)), ((rowB0, colB0), (rowBn, colBn)))
                | abs (rowAn - rowA0) == abs (colBn - colB0) ->
                    Just
                        ( Matrix
                            ( Array.array
                                ((rowB0, colA0), (rowBn, rowAn))
                                [((i, j), sum $ zipWith (*) (row a i) (col b j)) | i <- [rowB0 .. rowBn], j <- [colA0 .. colAn]]
                            )
                        )
                | otherwise -> Nothing
