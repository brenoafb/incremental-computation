module Rdg where

import Text.Show.Functions ()

data Rdg a =
  Rdg { nodeValue :: a
      , cache :: Maybe a
      , op :: (a -> a -> a)
      , dependencies :: [Rdg a]
      } deriving Show

executeRdg :: Rdg a -> a
executeRdg r = fetchCache r'
  where r' = propagateRdg r

propagateRdg :: Rdg a -> Rdg a
propagateRdg (Rdg x c f rs) =
  case c of
    Just _ -> Rdg x c f rs
    Nothing -> Rdg x (Just result) f rs'
      where rs' = map propagateRdg rs
            result = foldr (\r acc -> fetchCache r `f` acc) x rs'

fetchValue :: Rdg a -> a
fetchValue (Rdg x _ _ _) = x

fetchCache :: Rdg a -> a
fetchCache (Rdg _ (Just x) _ _) = x
