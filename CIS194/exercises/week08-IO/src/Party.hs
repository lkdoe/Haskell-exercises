module Party where

import Data.List (intercalate, sort)
import Data.Monoid
import Data.Tree
import Employee

------------------------------------------------------------
exampleTree :: Tree Int
exampleTree = Node 1 [Node 2 [Node 3 []], Node 4 []]

exampleGL1 :: GuestList
exampleGL1 = foldr glCons mempty [Emp name fun | name <- ["a", "b", "c"], fun <- [1 .. 3]]

exampleGL2 :: GuestList
exampleGL2 = foldr glCons mempty [Emp name fun | name <- ["d", "e", "f", "g"], fun <- [1 .. 2]]

------------------------------------------------------------
-- # Exerise 1

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp{empFun = fun}) (GL emps funSum) = GL (e : emps) (funSum + fun)

instance Semigroup GuestList where
    (GL e1 f1) <> (GL e2 f2) = GL (e1 ++ e2) (f1 + f2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Helper function for combining a list of guest lists into one
glCombine :: [GuestList] -> GuestList
glCombine [] = mempty
glCombine [gl] = gl
glCombine (gl : gls) = gl <> glCombine gls

------------------------------------------------------------
-- # Exerise 2
-- implemend a fold for Tree a from Data.Tree
-- The information given in the assignment is wrong, since Tree is an instance of foldable.
-- It works perfectly fine with foldr and foldl.
-- See also https://hackage-content.haskell.org/package/containers-0.8/docs/Data-Tree.html

treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold f b (Node a []) = f a b
-- treeFold f b (Node a [a1]) = treeFold f (f a b) a1
-- treeFold f b (Node a as) = foldr (\t1 b1 -> treeFold f b1 t1) (f a b) as
treeFold f b (Node a as) = foldr (flip (treeFold f)) (f a b) as

treeFold' :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold' f i (Node{rootLabel = rl, subForest = sf}) = f rl (map (treeFold' f i) sf)

-- This function makes the following assumptions in its arguments:
-- First input: Employee is the head of a division
-- Second input: [GuestList] is a list of the optimal GuestLists
--   of subdivisions withing the division
-- Output is optimized GuestList for the entire division
-- As stated in the assignment, this will not work.
-- combineGLs :: Employee-> [GuestList]-> GuestList

--------------------------------------------------
-- # Exerise 3
-- nextLevel takes the following:
-- A "boss" of the current subtree
-- A list of pairs constructed from each sub(tree)division under the "boss" in the company.
-- The first of each pair is the the best list with its own head of division.
-- The second of each pair is the best list without its own head of division.
-- The first of the output is the best list with the "boss" and the second is the best list without them.
-- glCombine always returns a GuestList, and an empty GuestList has Fun == 0.
-- This makes nextLevel safe.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls =
    let divs = unzip gls
     in case divs of
            (hasHead, hasNoHead) ->
                (glCons boss (glCombine hasNoHead), max (glCombine hasHead) (glCombine hasNoHead))

-- Since I chose to make treeFold :: (a -> b -> b) -> b -> Tree a -> b,
-- it cannot fold Trees with nextLevel.
-- So I need some function (a -> [b] -> b) -> (a -> [b] -> [b}).

--------------------------------------------------
-- # Exerise 4
-- Idea: Make the list of subtrees into lists of pairs of optimum guest lists.
-- Use them for nextLevel and take the max of its fst and snd.
-- This solution might not be optimal but it seems to work.

maxFun :: Tree Employee -> GuestList
-- maxFun (Node boss []) = glCons boss mempty
maxFun company = uncurry max best
  where
    best =
        let opts = unzip bestLists
         in case opts of
                (withBoss, withoutBoss) -> (glCombine withBoss, glCombine withoutBoss)
    bestLists = treeFold (\e l -> [nextLevel e l]) [(mempty, mempty)] company

------------------------------------------------------------
-- # Exercise 5

inOutExample = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

formatGuests :: [Employee] -> String
formatGuests [] = "Nobody"
formatGuests [g] = empName g ++ "\n"
formatGuests (g : gs) = empName g ++ "\n" ++ formatGuests gs

listEmpNames :: [Employee] -> [Name]
listEmpNames = map empName

-- Here, intercalate "\n" could be exchanged with unlines. It does the job though.
formatGL :: GuestList -> String
formatGL (GL [] _) = "Total fun: 0 \nNobody invited \n"
formatGL (GL guests fun) =
    "Total fun: " ++ show fun ++ "\n" ++ intercalate "\n" (sort $ map empName guests) ++ "\n"

main = do
    companyAsText <- readFile "company.txt"
    let companyAsTree = read companyAsText :: Tree Employee
    let optimumList = maxFun companyAsTree
    putStrLn $ formatGL optimumList
