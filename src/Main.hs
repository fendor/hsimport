
module Main where

import System.Exit (exitFailure, exitSuccess)
import HsImport

main :: IO ()
main = do
   args      <- hsImportArgs
   maybeSpec <- hsImportSpec args
   case maybeSpec of
        Left  error -> putStrLn ("hsimport: " ++ error) >> exitFailure
        Right spec  -> hsImport spec                    >> exitSuccess
