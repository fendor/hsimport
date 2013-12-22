{-# LANGUAGE DeriveDataTypeable #-}

module HsImport.Args 
   ( HsImportArgs(..)
   , hsImportArgs
   ) where

import System.Console.CmdArgs


data HsImportArgs = HsImportArgs {
   moduleName :: String,
   symbolName :: String,
   sourceFile :: FilePath
   } deriving (Data, Typeable, Show, Eq)


hsImportArgs :: IO HsImportArgs
hsImportArgs = cmdArgs $ HsImportArgs {
   moduleName = def &= help "The module to import",
   symbolName = def &= help "The symbol to import",
   sourceFile = def &= args &= typ "SOURCEFILE"
   }
   &= program "hsimport"
   &= summary summaryInfo
   &= help "A command line programm for extending the import list of a haskell source file."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo]
   where
      versionInfo = "hsimport version 0.1"
      summaryInfo = ""
