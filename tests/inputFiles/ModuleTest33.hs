{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
-- | Some Haddock doc
--   One line more
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
