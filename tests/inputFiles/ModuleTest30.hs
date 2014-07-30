{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Ugah.Foo
import Control.Applicative
import Ugah.Blub ( a
                 , b
                 )
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
