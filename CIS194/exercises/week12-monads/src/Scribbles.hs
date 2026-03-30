module Scribbles where

import Control.Monad.Random

threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
    getRandom >>= \i1 ->
        getRandom >>= \i2 ->
            getRandom >>= \i3 ->
                return (i1, i2, i3)
