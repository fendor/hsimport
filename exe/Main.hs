
module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import HsImport

main :: IO ()
main = do
   args      <- hsImportArgs
   maybeSpec <- hsImportSpec args
   case maybeSpec of
        Left  error -> hPutStrLn stderr ("hsimport: " ++ error) >> exitFailure
        Right spec  -> hsImport spec                            >> exitSuccess
