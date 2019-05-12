{-# Language PatternGuards #-}
module Blub
   ( f
   ) where
#include "Foo.h"
import Ugah.Foo
import Control.Applicative
#ifdef FOO
import Ugah.Blub
#endif
f :: Int -> Int
f i = i + 3
