
module HsImport.Config
   ( Config(..)
   , defaultConfig
   ) where

import qualified HsImport.PrettyPrint as PP
import qualified HsImport.ImportPos as IP
import HsImport.Types

-- | User definable configuration for hsImport.
data Config = Config
   { -- | function for pretty printing of the import declarations
     prettyPrint :: ImportDecl -> String
     -- | function for finding the position of new import declarations
   , findImportPos :: ImportDecl -> [ImportDecl] -> Maybe IP.ImportPos
     -- | error during configuration of hsimport
   , configError :: Maybe String
   }


defaultConfig :: Config
defaultConfig = Config
   { prettyPrint   = PP.prettyPrint
   , findImportPos = IP.findImportPos
   , configError   = Nothing
   }
