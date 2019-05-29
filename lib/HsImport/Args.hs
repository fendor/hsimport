{-# LANGUAGE DeriveDataTypeable, CPP #-}

module HsImport.Args
   ( HsImportArgs(..)
   , hsImportArgs
   , defaultArgs
   ) where

import Prelude hiding (all)
import System.Console.CmdArgs

#ifdef CABAL
import Data.Version (showVersion)
import Paths_hsimport (version)
#endif

data HsImportArgs = HsImportArgs
   { moduleName    :: String
   , symbolName    :: String
   , hide          :: Bool
   , all           :: Bool
   , with          :: [String]
   , qualifiedName :: String
   , as            :: String
   , inputSrcFile  :: FilePath
   , outputSrcFile :: FilePath
   } deriving (Data, Typeable, Show, Eq)


hsImportArgs :: IO HsImportArgs
hsImportArgs = cmdArgs $ HsImportArgs
   { moduleName    = def &= help "The module to import"
   , symbolName    = def &= help "The symbol to import, if empty, the entire module is imported"
   , hide          = def &= help "Hide the given symbols in the import"
                         &= name "hide"
   , all           = def &= help "All constructors or methods of the symbol should be imported: 'Symbol(..)'"
                         &= name "all" &= name "a"
   , with          = def &= help "The constructors or methods of the symbol should be imported: 'Symbol(With)'"
   , qualifiedName = def &= help "The name to use for a qualified module import"
   , as            = def &= help "The name to use for an unqualified module import" &= name "as"
   , outputSrcFile = def &= help "Save modified source file to file, if empty, the source file is modified inplace" &= typFile
   , inputSrcFile  = def &= args &= typ "SOURCEFILE"
   }
   &= program "hsimport"
   &= summary summaryInfo
   &= help "A command line program for extending the import list of a Haskell source file."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo]
   where
      summaryInfo = ""


defaultArgs :: HsImportArgs
defaultArgs = HsImportArgs
   { moduleName    = def
   , symbolName    = def
   , hide          = False
   , all           = def
   , with          = def
   , qualifiedName = def
   , as            = def
   , inputSrcFile  = def
   , outputSrcFile = def
   }


versionInfo :: String
versionInfo =
#ifdef CABAL
   "hsimport version " ++ showVersion version
#else
   "hsimport version unknown (not built with cabal)"
#endif
