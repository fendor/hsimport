{-# LANGUAGE DeriveDataTypeable #-}

module HsImport.Args 
   ( HsImportArgs(..)
   , hsImportArgs
   ) where

import System.Console.CmdArgs


data HsImportArgs = HsImportArgs 
   { moduleName    :: String
   , symbolName    :: String
   , inputSrcFile  :: FilePath
   , outputSrcFile :: FilePath
   } deriving (Data, Typeable, Show, Eq)


hsImportArgs :: IO HsImportArgs
hsImportArgs = cmdArgs $ HsImportArgs 
   { moduleName    = def &= help "The module to import"
   , symbolName    = def &= help "The symbol to import, if empty, the entire module is imported"
   , outputSrcFile = def &= help "Save modified source file to file, if empty, the source file is modified inplace" &= typFile
   , inputSrcFile  = def &= args &= typ "SOURCEFILE"
   }
   &= program "hsimport"
   &= summary summaryInfo
   &= help "A command line program for extending the import list of a Haskell source file."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo]
   where
      versionInfo = "hsimport version 0.2"
      summaryInfo = ""
