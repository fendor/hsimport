{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
import Ugah.Foo
import Control.Applicative
import qualified Control.Monad as CM
import Ugah.Blub
f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
