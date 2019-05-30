
module HsImport.ModuleImport
   ( ModuleImport(..)
   , Name
   ) where

type Name = String

-- | How the module should be imported
data ModuleImport = ModuleImport
   { moduleName :: Name       -- ^ the name of the module to import
   , qualified  :: Bool       -- ^ if the module should be imported qualified
   , as         :: Maybe Name -- ^ the module should be imported with this name
   } deriving (Show)
