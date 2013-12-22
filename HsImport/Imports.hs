
module HsImport.Imports 
   ( matchingImports
   , entireModuleImported
   , symbolImported
   ) where

import Data.Maybe
import qualified Language.Haskell.Exts as HS
type Imports = [HS.ImportDecl]


matchingImports :: String -> HS.Module -> Imports
matchingImports moduleName (HS.Module _ _ _ _ _ imports _) =
   [ i 
   | i@HS.ImportDecl {HS.importModule = HS.ModuleName name} <- imports
   , moduleName == name
   ] 


entireModuleImported :: HS.ImportDecl -> Bool
entireModuleImported import_ =
   (not $ HS.importQualified import_) && (isNothing $ HS.importSpecs import_)


symbolImported :: String -> HS.ImportDecl -> Bool
symbolImported symbolName import_ = hasSymbol symbolName import_
   where
      hasSymbol sym (HS.ImportDecl {HS.importSpecs = Just (False, symbols)}) = any (hasIVar sym) symbols
      hasSymbol _   _                                                        = False

      hasIVar sym (HS.IVar (HS.Ident  id)) = sym == id
      hasIVar sym (HS.IVar (HS.Symbol  s)) = sym == s
      hasIVar _   _                        = False
