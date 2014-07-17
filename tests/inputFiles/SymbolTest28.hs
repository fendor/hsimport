{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Control.Applicative
import Ugah.Blub ( a
                 , b
                 , c
                 )
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
