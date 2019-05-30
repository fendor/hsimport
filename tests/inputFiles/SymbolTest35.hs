{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Applicative
   (r, t, z)
import Data.Text hiding (isInfixOf)
import Ugah.Blub
   ( a
   , b
   , c
   )
f :: Int -> Int
f = (+ 3)

r :: Int -> Int
r =
