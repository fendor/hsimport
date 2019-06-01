{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Applicative
   (r, t, z)
import Control.Monad hiding (when)
import Ugah.Blub
   ( a
   , b
   , c
   )
f :: Int -> Int
f = (+ 3)

r :: Int -> Int
r =
