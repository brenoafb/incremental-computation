module Ptr where

import Rdg
import qualified RdgZipper as Z

data Ptr a = Ptr [a] a [a]
  deriving (Eq, Show)

mkPtr :: [a] -> Ptr a
mkPtr (x:xs) = Ptr [] x xs

forward :: Ptr a -> Ptr a
forward (Ptr bs x (a:as)) = Ptr (x:bs) a as

backward :: Ptr a -> Ptr a
backward (Ptr (b:bs) x as) = Ptr bs b (x:as)

modify :: a -> Ptr a -> Ptr a
modify x (Ptr bs _ as) = Ptr bs x as

insertFront :: a -> Ptr a -> Ptr a
insertFront x (Ptr bs y as) = Ptr (x:bs) y as

insertAfter :: a -> Ptr a -> Ptr a
insertAfter x (Ptr bs y as) = Ptr bs y (x:as)

rewind :: Ptr a -> [a]
rewind (Ptr [] x as) = x:as
rewind (Ptr (b:bs) x as) = rewind (Ptr bs b (x:as))

type MPtr a = (Ptr a, Z.Location a)

mkMPtr :: [a] -> Rdg a -> MPtr a
mkMPtr xs rdg = (mkPtr xs, Z.mkloc rdg)

mforward :: MPtr a -> MPtr a
mforward (ptr, loc) = (forward ptr, Z.down loc)

mbackward :: MPtr a -> MPtr a
mbackward (ptr, loc) = (backward ptr, Z.up loc)

mchange :: a -> MPtr a -> MPtr a
mchange x (ptr, loc) = (modify x ptr, Z.modify x loc)

mrewind :: MPtr a -> ([a], Rdg a)
mrewind (ptr, loc) = (rewind ptr, Z.rewind (Z.invalidateCache loc))
