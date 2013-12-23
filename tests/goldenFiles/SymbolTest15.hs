module Test where
import Foo.Bar
import Foo.Bar.Blub
import Ugah.Argh
import Control.Applicative (pure, (<$>), (<*), (*>))

f :: Int -> Int
f = (+ 3)
