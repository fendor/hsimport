{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Ugah.Foo
import Control.Applicative ( a
                           , b
                           )
import Control.Monad
import Ugah.Blub
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
