{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Applicative
   (r, t, z)
import Ugah.Blub
   ( a
   , b
   , c
   )
import Data.Text hiding (isPrefixOf, isInfixOf)
f :: Int -> Int
f = (+ 3)

r :: Int -> Int
r =
