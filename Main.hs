
module Main where

import HsImport


main :: IO ()
main = do
   args      <- hsImportArgs
   maybeSpec <- mkImportSpec args
   case maybeSpec of
        Left  error -> putStrLn $ "hsimport: " ++ error
        Right spec  -> hsImport spec
