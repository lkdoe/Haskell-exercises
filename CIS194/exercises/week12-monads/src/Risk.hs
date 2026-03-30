{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
    deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
    random = first DV . randomR (1, 6)
    randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- Army denotes the number of armies an attacking or defending player has in a given country.
type Army = Int

-- The attacking and defending armies in a given Battlefield can be more than 3.
data Battlefield = Battlefield {attackers :: Army, defenders :: Army}
    deriving (Show)

--------------------------------------------------------------------------------
-- # Exercise 1
--------------------------------------------------------------------------------
-- Install the MonadRandom module.
-- This is required for the import of the Control.Monad.Random module.

--------------------------------------------------------------------------------
-- # Exercise 2
--------------------------------------------------------------------------------
-- A maximum of three armies can attack in a given battle.
-- At least one army of the attacking side must stay behind and cannot participate in the battle.
-- The number of defending armies is limited by the number of attacking armies.
-- Assumption: All attacks and defences use the maximum allowed number of armies.
battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield att def) =
    let
        throwDice n = sort <$> replicateM n die
        a = throwDice (min 3 (att - 1))
        d = throwDice (min 2 def)
        compareDice bf [] _ = bf
        compareDice bf _ [] = bf
        compareDice (Battlefield atts defs) (a0 : as) (d0 : ds)
            | a0 > d0 = compareDice (Battlefield atts (defs - 1)) as ds
            | otherwise = compareDice (Battlefield (atts - 1) defs) as ds
     in
        liftM2 (compareDice b) a d

showBattle :: Rand StdGen Battlefield -> IO (Army, Army)
showBattle b = (\f -> (attackers f, defenders f)) <$> evalRandIO b

--------------------------------------------------------------------------------
-- # Exercise 3
--------------------------------------------------------------------------------

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
    | a <= 1 || d <= 0 = return b
    | otherwise = battle b >>= invade

--------------------------------------------------------------------------------
-- # Exercise 3
--------------------------------------------------------------------------------
successProb :: Battlefield -> Rand StdGen Double
successProb b = (/ 1000) . fromIntegral . length . filter ((> 1) . attackers) <$> replicateM 1000 (invade b)

isWin :: Battlefield -> Integer
isWin b = if defenders b == 0 && attackers b > 1 then 1 else 0
