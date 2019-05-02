{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Monad
f :: Int -> Int
-- ^ Some Haddock doc
f = (+ 3)

g :: Int -> Int
g =
   where
