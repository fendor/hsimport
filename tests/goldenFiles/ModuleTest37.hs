{-# Language PatternGuards #-}
module Blub
   ( blub
   , foo
   , bar
   ) where
#include "Foo.h"
#ifdef FOO
import Control.Applicative
#endif

import Control.BiFunctor
import Control.Monad

f :: Int -> Int
-- ^ Some Haddock doc
--   One line more
f = (+ 3)

g :: Int -> Int
g =
   where
