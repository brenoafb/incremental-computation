module RdgZipper where

import Rdg
import Text.Show.Functions ()

data Path a = Top
            | Node [Rdg a] (Path a) [Rdg a]
            deriving Show

type DataStack a = [(a, Maybe a, (a -> a -> a))]

data Location a = Loc (DataStack a) (Rdg a) (Path a)
  deriving Show

mkloc :: Rdg a -> Location a
mkloc r = Loc [] r Top

left :: Location a -> Location a
left (Loc ds rdg p) =
  case p of
    Top -> error "left of top"
    Node (l:ls) u rs -> Loc ds l (Node ls u (rdg:rs))

right :: Location a -> Location a
right (Loc ds rdg p) =
  case p of
    Top -> error "right of top"
    Node ls u (r:rs) -> Loc ds r (Node (rdg:ls) u rs)

up :: Location a -> Location a
up (Loc ds rdg p) =
  case p of
    Top -> error "up to top"
    Node ls u rs -> Loc ds' rdg' u
      where rdg' = Rdg x c op ((reverse ls) ++ [rdg] ++ rs)
            ((x,c,op):ds') = ds

down :: Location a -> Location a
down (Loc ds x p) =
  case x of
    Rdg _ _ _ [] -> error "Down of leaf"
    Rdg y c op (r:rs) -> Loc ((y,c,op):ds) r (Node [] p rs)

rewind :: Location a -> Rdg a
rewind loc = rdg
  where
    Loc _ rdg _ = go loc
    go (Loc ds rdg p) =
      case p of
        Node _ _ _ -> up (Loc ds rdg' p)
        Top -> Loc ds rdg' Top
      where (Rdg x _ op rs) = rdg
            rdg' = Rdg x Nothing op rs

modify :: a -> Location a -> Location a
modify x (Loc ds rdg p) =
  case rdg of
    Rdg _ c op xs -> Loc ds rdg' p
      where rdg' = Rdg x c op xs

invalidateCache :: Location a -> Location a
invalidateCache (Loc ds rdg p) =
  case p of
    Node _ _ _ -> up (Loc ds rdg' p)
    Top -> Loc ds rdg' Top
  where (Rdg x _ op rs) = rdg
        rdg' = Rdg x Nothing op rs
