{-# Language PatternGuards #-}

module HsImport.ImportChange
   ( ImportChange(..)
   , importChange
   ) where

import Data.Maybe
import Data.List (find)
import Data.List.Split (splitOn)
import Control.Lens
import qualified Language.Haskell.Exts as HS
import qualified Data.Attoparsec.Text as A

data ImportChange = ReplaceImport HS.ImportDecl
                  | AddImport HS.ImportDecl
                  | NoImportChange
                  deriving (Show)


importChange :: String -> Maybe String -> HS.Module -> ImportChange
importChange moduleName (Just symbolName) module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching || any (symbolImported symbolName) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl -> ReplaceImport $ addSymbol impDecl symbolName
                   Nothing      -> AddImport $ importDeclWithSymbol (HS.importLoc . last $ matching) moduleName symbolName

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImport $ importDeclWithSymbol (HS.importLoc bestMatch) moduleName symbolName

   | otherwise = AddImport $ importDeclWithSymbol (lastImportSrcLoc module_) moduleName symbolName

   where
      addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbolName =
         id {HS.importSpecs = specs & _Just . _2 %~ (++ [HS.IVar $ hsName symbolName])}

importChange moduleName Nothing module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching
         then NoImportChange
         else AddImport $ importDecl (HS.importLoc . last $ matching) moduleName

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImport $ importDecl (HS.importLoc bestMatch) moduleName

   | otherwise = AddImport $ importDecl (lastImportSrcLoc module_) moduleName


matchingImports :: String -> HS.Module -> [HS.ImportDecl]
matchingImports moduleName (HS.Module _ _ _ _ _ imports _) =
   [ i 
   | i@HS.ImportDecl {HS.importModule = HS.ModuleName name} <- imports
   , moduleName == name
   ] 


bestMatchingImport :: String -> HS.Module -> Maybe HS.ImportDecl
bestMatchingImport moduleName (HS.Module _ _ _ _ _ imports _) =
   case ifoldl' computeMatches Nothing splittedMods of
        Just (idx, _) -> Just $ imports !! idx
        _             -> Nothing
   where
      computeMatches idx matches mod =
         let num' = numMatches splittedMod mod
             in case matches of
                     Just (_, num) | num' >= num -> Just (idx, num')
                                   | otherwise   -> matches

                     Nothing | num' > 0  -> Just (idx, num')
                             | otherwise -> Nothing
         where
            numMatches as bs = loop 0 as bs
               where
                  loop num (a:as) (b:bs)
                     | a == b    = loop (num + 1) as bs
                     | otherwise = num

                  loop num [] _ = num
                  loop num _ [] = num

      splittedMod  = splitOn "." moduleName
      splittedMods = [ splitOn "." name 
                     | HS.ImportDecl {HS.importModule = HS.ModuleName name} <- imports
                     ]  


lastImportSrcLoc :: HS.Module -> HS.SrcLoc
lastImportSrcLoc (HS.Module srcLoc _ _ _ _ imports _)
   | [] <- imports = srcLoc
   | otherwise     = HS.importLoc . last $ imports


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


hasImportedSymbols :: HS.ImportDecl -> Bool
hasImportedSymbols import_
   | (HS.ImportDecl {HS.importSpecs = Just (False, _)}) <- import_ = True
   | otherwise                                                     = False


importDecl :: HS.SrcLoc -> String -> HS.ImportDecl
importDecl srcLoc moduleName = HS.ImportDecl 
   { HS.importLoc       = srcLoc
   , HS.importModule    = HS.ModuleName moduleName
   , HS.importQualified = False
   , HS.importSrc       = False
   , HS.importPkg       = Nothing
   , HS.importAs        = Nothing
   , HS.importSpecs     = Nothing
   }


importDeclWithSymbol :: HS.SrcLoc -> String -> String -> HS.ImportDecl
importDeclWithSymbol srcLoc moduleName symbolName = 
   (importDecl srcLoc moduleName) { HS.importSpecs = Just (False, [HS.IVar $ hsName symbolName]) }


hsName :: String -> HS.Name
hsName symbolName
   | isSymbol  = HS.Symbol symbolName
   | otherwise = HS.Ident symbolName
   where
      isSymbol = any (A.notInClass "a-zA-Z0-9_") symbolName
