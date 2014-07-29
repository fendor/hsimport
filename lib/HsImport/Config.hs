
module HsImport.Config
   ( Config(..)
   , defaultConfig
   ) where

import qualified Language.Haskell.Exts as HS
import qualified HsImport.PrettyPrint as PP


-- | user definable configuration for hsImport
data Config = Config
   { prettyPrint :: HS.ImportDecl -> String  -- ^ function for pretty printing of the import declarations
   , configError :: Maybe String             -- ^ error during configuration of hsImport
   }


defaultConfig :: Config
defaultConfig = Config
   { prettyPrint = PP.prettyPrint
   , configError = Nothing
   }
