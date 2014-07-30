{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Ugah.Foo ( a
                , b
                )
import Control.Applicative
import Ugah.Blub
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
