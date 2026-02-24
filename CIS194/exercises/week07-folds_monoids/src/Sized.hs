{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sized where

import Data.Monoid

newtype Size = Size Int
    deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

class Sized a where
    size :: a -> Size

instance Sized Size where
    size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance (Sized b) => Sized (a, b) where
    size = size . snd

-- Since GHC 9.4, instance of Semigroup needs to be declared.
-- Monoid is now a subtype of Semigroup.
instance Semigroup Size where
    (<>) = (+)

instance Monoid Size where
    mempty = Size 0

-- Declaration of (<>) in the Semigroup is sufficient;
-- mappend is synonym.
--    mappend = (+)
