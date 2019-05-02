{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
-- | Some Haddock doc
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
