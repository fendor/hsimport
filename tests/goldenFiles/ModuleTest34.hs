{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Monad
{-|
  Some Haddock doc
  One line more
-}
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
