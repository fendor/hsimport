{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
f :: Int -> Int
-- ^ Some Haddock doc
--   One line more
f = (+ 3)

g :: Int -> Int
g =
   where
