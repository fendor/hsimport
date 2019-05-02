{-# Language PatternGuards #-}

module HsImport.ImportChange
   ( ImportChange(..)
   , importChanges
   ) where

import Data.Maybe
import Data.List (find, (\\))
import Lens.Micro
import qualified Language.Haskell.Exts as HS
import qualified Data.Attoparsec.Text as A
import HsImport.SymbolImport (SymbolImport(..))
import HsImport.ModuleImport (ModuleImport(..))
import HsImport.ImportPos (matchingImports)
import HsImport.Utils
import HsImport.Types

-- | How the import declarations should be changed
data ImportChange
   = ReplaceImportAt SrcSpan ImportDecl -- ^ replace the import declaration at SrcSpan
   | AddImportAfter SrcLine ImportDecl  -- ^ add import declaration after SrcLine
   | AddImportAtEnd ImportDecl          -- ^ add import declaration at end of source file
   | FindImportPos ImportDecl           -- ^ search for an insert position for the import declaration
   | NoImportChange                     -- ^ no changes of the import declarations
   deriving (Show)


importChanges :: ModuleImport -> Maybe SymbolImport -> Module -> [ImportChange]
importChanges (ModuleImport moduleName False Nothing) Nothing hsModule =
   [ importModule moduleName hsModule ]

importChanges (ModuleImport moduleName False Nothing) (Just symbolImport) hsModule =
   [ importModuleWithSymbol moduleName symbolImport hsModule ]

importChanges (ModuleImport moduleName qualified as) symbolImport hsModule =
   [ maybe NoImportChange (\sym -> importModuleWithSymbol moduleName sym hsModule) symbolImport

   , if qualified
        then importQualifiedModule moduleName (maybe moduleName id as) hsModule
        else maybe NoImportChange (\asName -> importModuleAs moduleName asName hsModule) as
   ]


importModule :: String -> Module -> ImportChange
importModule moduleName module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any entireModuleImported matching
         then NoImportChange
         else FindImportPos $ importDecl moduleName

   | not $ null (importDecls module_) =
      FindImportPos $ importDecl moduleName

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDecl moduleName)
           Nothing      -> AddImportAtEnd (importDecl moduleName)


importModuleWithSymbol :: String -> SymbolImport -> Module -> ImportChange
importModuleWithSymbol moduleName symbolImport module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any entireModuleImported matching || any (symbolImported symbolImport) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl ->
                      ReplaceImportAt (srcSpan . HS.ann $ impDecl) (addSymbol impDecl symbolImport)

                   Nothing      ->
                      FindImportPos $ importDeclWithSymbol moduleName symbolImport

   | not $ null (importDecls module_) =
      FindImportPos $ importDeclWithSymbol moduleName symbolImport

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDeclWithSymbol moduleName symbolImport)
           Nothing      -> AddImportAtEnd (importDeclWithSymbol moduleName symbolImport)
   where
      addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbolImport =
         id {HS.importSpecs = specs & _Just %~ extendSpecList symbolImport}

      extendSpecList symbolImport (HS.ImportSpecList srcSpan hid specs) =
         HS.ImportSpecList srcSpan hid (specs ++ [importSpec symbolImport])


importQualifiedModule :: String -> String -> Module -> ImportChange
importQualifiedModule moduleName qualifiedName module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any (hasQualifiedImport qualifiedName) matching
         then NoImportChange
         else FindImportPos $ qualifiedImportDecl moduleName qualifiedName

   | not $ null (importDecls module_) =
      FindImportPos $ qualifiedImportDecl moduleName qualifiedName

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (qualifiedImportDecl moduleName qualifiedName)
           Nothing      -> AddImportAtEnd (qualifiedImportDecl moduleName qualifiedName)


importModuleAs :: String -> String -> Module -> ImportChange
importModuleAs moduleName asName module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any (hasAsImport asName) matching
         then NoImportChange
         else FindImportPos $ asImportDecl moduleName asName

   | not $ null (importDecls module_) =
      FindImportPos $ asImportDecl moduleName asName

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (asImportDecl moduleName asName)
           Nothing      -> AddImportAtEnd (asImportDecl moduleName asName)


entireModuleImported :: ImportDecl -> Bool
entireModuleImported import_ =
   not (HS.importQualified import_) && isNothing (HS.importSpecs import_)


hasQualifiedImport :: String -> ImportDecl -> Bool
hasQualifiedImport qualifiedName import_
   | HS.importQualified import_
   , Just (HS.ModuleName _ importAs) <- HS.importAs import_
   , importAs == qualifiedName
   = True

   | otherwise = False


hasAsImport :: String -> ImportDecl -> Bool
hasAsImport asName import_
   | not $ HS.importQualified import_
   , Just (HS.ModuleName _ importAs) <- HS.importAs import_
   , importAs == asName
   = True

   | otherwise
   = False


symbolImported :: SymbolImport -> ImportDecl -> Bool
symbolImported symbolImport import_
   | Just (HS.ImportSpecList _ False hsSymbols) <- HS.importSpecs import_
   , any (imports symbolImport) hsSymbols
   = True

   | otherwise
   = False
   where
      imports (Symbol symName)             (HS.IVar _ name)                    = symName == nameString name
      imports (Symbol symName)             (HS.IAbs _ _ name)                  = symName == nameString name
      imports (Symbol symName)             (HS.IThingAll _ name)               = symName == nameString name
      imports (Symbol symName)             (HS.IThingWith _ name _)            = symName == nameString name
      imports (AllOfSymbol symName)        (HS.IThingAll _ name)               = symName == nameString name
      imports (SomeOfSymbol symName _    ) (HS.IThingAll _ name)               = symName == nameString name
      imports (SomeOfSymbol symName names) (HS.IThingWith _ hsSymName hsNames) =
         symName == nameString hsSymName && null (names \\ (map (nameString . toName) hsNames))

      imports _ _ = False

      nameString (HS.Ident _ id)   = id
      nameString (HS.Symbol _ sym) = sym

      toName (HS.VarName _ name) = name
      toName (HS.ConName _ name) = name


hasImportedSymbols :: ImportDecl -> Bool
hasImportedSymbols import_
   | Just (HS.ImportSpecList _ False (_:_)) <- HS.importSpecs import_
   = True

   | otherwise
   = False

importDecl :: String -> ImportDecl
importDecl moduleName = HS.ImportDecl
   { HS.importAnn       = noAnnotation
   , HS.importModule    = HS.ModuleName noAnnotation moduleName
   , HS.importQualified = False
   , HS.importSrc       = False
   , HS.importSafe      = False
   , HS.importPkg       = Nothing
   , HS.importAs        = Nothing
   , HS.importSpecs     = Nothing
   }


importDeclWithSymbol :: String -> SymbolImport -> ImportDecl
importDeclWithSymbol moduleName symbolImport =
   (importDecl moduleName) { HS.importSpecs = Just (HS.ImportSpecList noAnnotation
                                                                      False
                                                                      [importSpec symbolImport])
                           }


qualifiedImportDecl :: String -> String -> ImportDecl
qualifiedImportDecl moduleName qualifiedName =
   (importDecl moduleName) { HS.importQualified = True
                           , HS.importAs        = if moduleName /= qualifiedName
                                                     then Just $ HS.ModuleName noAnnotation qualifiedName
                                                     else Nothing
                           }


asImportDecl :: String -> String -> ImportDecl
asImportDecl moduleName asName =
   (importDecl moduleName) { HS.importQualified = False
                           , HS.importAs        = Just $ HS.ModuleName noAnnotation asName
                           }


importSpec :: SymbolImport -> ImportSpec
importSpec (Symbol symName)             = HS.IVar noAnnotation (hsName symName)
importSpec (AllOfSymbol symName)        = HS.IThingAll noAnnotation (hsName symName)
importSpec (SomeOfSymbol symName names) = HS.IThingWith noAnnotation
                                                        (hsName symName)
                                                        (map ((HS.VarName noAnnotation) . hsName) names)


hsName :: String -> Name
hsName symbolName
   | isSymbol  = HS.Symbol noAnnotation symbolName
   | otherwise = HS.Ident noAnnotation symbolName
   where
      isSymbol = any (A.notInClass "a-zA-Z0-9_'") symbolName


srcLineForNewImport :: Module -> Maybe SrcLine
srcLineForNewImport module_ =
   case module_ of
        HS.Module ann _ _ imports decls            -> newSrcLine ann imports decls
        HS.XmlPage _ _ _ _ _ _ _                   -> Nothing
        HS.XmlHybrid ann _ _ imports decls _ _ _ _ -> newSrcLine ann imports decls
   where
      newSrcLine :: Annotation -> [ImportDecl] -> [Decl] -> Maybe SrcLine
      newSrcLine ann imports decls
         | not $ null imports
         = Just (firstSrcLine . HS.ann $ last imports)

         | (decl:_) <- decls
         , sLoc <- declSrcLoc decl
         , HS.srcLine sLoc >= firstSrcLine ann
         = Just $ max 0 (HS.srcLine sLoc - 1)

         | otherwise
         = Nothing
