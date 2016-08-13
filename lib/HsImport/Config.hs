
module HsImport.Config
   ( Config(..)
   , defaultConfig
   ) where

import qualified Language.Haskell.Exts as HS
import qualified HsImport.PrettyPrint as PP
import qualified HsImport.ImportPos as IP

type HsImportDecl = HS.ImportDecl HS.SrcSpanInfo

-- | User definable configuration for hsImport.
data Config = Config
   { -- | function for pretty printing of the import declarations
     prettyPrint :: HsImportDecl -> String
     -- | function for finding the position of new import declarations
   , findImportPos :: HsImportDecl -> [HsImportDecl] -> Maybe IP.ImportPos
     -- | error during configuration of hsimport
   , configError :: Maybe String
   }


defaultConfig :: Config
defaultConfig = Config
   { prettyPrint   = PP.prettyPrint
   , findImportPos = IP.findImportPos
   , configError   = Nothing
   }
