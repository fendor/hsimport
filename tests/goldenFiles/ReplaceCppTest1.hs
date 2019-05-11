{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
#include "Foo.h"
import Ugah.Foo
import Control.Applicative



f :: Int -> Int
f = (+ 3)

g :: Int -> Int
g =
   where
