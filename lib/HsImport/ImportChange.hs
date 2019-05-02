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
import HsImport.Symbol (Symbol(..))
import HsImport.ModuleImport (ModuleImport(..))
import HsImport.ImportPos (matchingImports)
import HsImport.Utils

type SrcLine      = Int
type HsImportDecl = HS.ImportDecl HS.SrcSpanInfo
type HsModule     = HS.Module HS.SrcSpanInfo

-- | How the import declarations should be changed
data ImportChange
   = ReplaceImportAt HS.SrcSpan HsImportDecl -- ^ replace the import declaration at SrcSpan
   | AddImportAfter SrcLine HsImportDecl     -- ^ add import declaration after SrcLine
   | AddImportAtEnd HsImportDecl             -- ^ add import declaration at end of source file
   | FindImportPos HsImportDecl              -- ^ search for an insert position for the import declaration
   | NoImportChange                          -- ^ no changes of the import declarations
   deriving (Show)


importChanges :: ModuleImport -> Maybe Symbol -> HsModule -> [ImportChange]
importChanges (ModuleImport moduleName False Nothing) Nothing hsModule =
   [ importModule moduleName hsModule ]

importChanges (ModuleImport moduleName False Nothing) (Just symbol) hsModule =
   [ importModuleWithSymbol moduleName symbol hsModule ]

importChanges (ModuleImport moduleName qualified as) symbol hsModule =
   [ maybe NoImportChange (\sym -> importModuleWithSymbol moduleName sym hsModule) symbol

   , if qualified
        then importQualifiedModule moduleName (maybe moduleName id as) hsModule
        else maybe NoImportChange (\asName -> importModuleAs moduleName asName hsModule) as
   ]


importModule :: String -> HsModule -> ImportChange
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


importModuleWithSymbol :: String -> Symbol -> HsModule -> ImportChange
importModuleWithSymbol moduleName symbol module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any entireModuleImported matching || any (symbolImported symbol) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl ->
                      ReplaceImportAt (srcSpan impDecl) (addSymbol impDecl symbol)

                   Nothing      ->
                      FindImportPos $ importDeclWithSymbol moduleName symbol

   | not $ null (importDecls module_) =
      FindImportPos $ importDeclWithSymbol moduleName symbol

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDeclWithSymbol moduleName symbol)
           Nothing      -> AddImportAtEnd (importDeclWithSymbol moduleName symbol)
   where
      addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbol =
         id {HS.importSpecs = specs & _Just %~ extendSpecList symbol}

      extendSpecList symbol (HS.ImportSpecList srcSpan hid specs) =
         HS.ImportSpecList srcSpan hid (specs ++ [importSpec symbol])


importQualifiedModule :: String -> String -> HsModule -> ImportChange
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


importModuleAs :: String -> String -> HsModule -> ImportChange
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


entireModuleImported :: HsImportDecl -> Bool
entireModuleImported import_ =
   not (HS.importQualified import_) && isNothing (HS.importSpecs import_)


hasQualifiedImport :: String -> HsImportDecl -> Bool
hasQualifiedImport qualifiedName import_
   | HS.importQualified import_
   , Just (HS.ModuleName _ importAs) <- HS.importAs import_
   , importAs == qualifiedName
   = True

   | otherwise = False


hasAsImport :: String -> HsImportDecl -> Bool
hasAsImport asName import_
   | not $ HS.importQualified import_
   , Just (HS.ModuleName _ importAs) <- HS.importAs import_
   , importAs == asName
   = True

   | otherwise
   = False


symbolImported :: Symbol -> HsImportDecl -> Bool
symbolImported symbol import_
   | Just (HS.ImportSpecList _ False hsSymbols) <- HS.importSpecs import_
   , any (imports symbol) hsSymbols
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


hasImportedSymbols :: HsImportDecl -> Bool
hasImportedSymbols import_
   | Just (HS.ImportSpecList _ False (_:_)) <- HS.importSpecs import_
   = True

   | otherwise
   = False

importDecl :: String -> HsImportDecl
importDecl moduleName = HS.ImportDecl
   { HS.importAnn       = HS.noSrcSpan
   , HS.importModule    = HS.ModuleName HS.noSrcSpan moduleName
   , HS.importQualified = False
   , HS.importSrc       = False
   , HS.importSafe      = False
   , HS.importPkg       = Nothing
   , HS.importAs        = Nothing
   , HS.importSpecs     = Nothing
   }


importDeclWithSymbol :: String -> Symbol -> HsImportDecl
importDeclWithSymbol moduleName symbol =
   (importDecl moduleName) { HS.importSpecs = Just (HS.ImportSpecList HS.noSrcSpan
                                                                      False
                                                                      [importSpec symbol])
                           }


qualifiedImportDecl :: String -> String -> HsImportDecl
qualifiedImportDecl moduleName qualifiedName =
   (importDecl moduleName) { HS.importQualified = True
                           , HS.importAs        = if moduleName /= qualifiedName
                                                     then Just $ HS.ModuleName HS.noSrcSpan qualifiedName
                                                     else Nothing
                           }


asImportDecl :: String -> String -> HsImportDecl
asImportDecl moduleName asName =
   (importDecl moduleName) { HS.importQualified = False
                           , HS.importAs        = Just $ HS.ModuleName HS.noSrcSpan asName
                           }


importSpec :: Symbol -> HS.ImportSpec HS.SrcSpanInfo
importSpec (Symbol symName)             = HS.IVar HS.noSrcSpan (hsName symName)
importSpec (AllOfSymbol symName)        = HS.IThingAll HS.noSrcSpan (hsName symName)
importSpec (SomeOfSymbol symName names) = HS.IThingWith HS.noSrcSpan
                                                        (hsName symName)
                                                        (map ((HS.VarName HS.noSrcSpan) . hsName) names)


hsName :: String -> HS.Name HS.SrcSpanInfo
hsName symbolName
   | isSymbol  = HS.Symbol HS.noSrcSpan symbolName
   | otherwise = HS.Ident HS.noSrcSpan symbolName
   where
      isSymbol = any (A.notInClass "a-zA-Z0-9_'") symbolName


srcLineForNewImport :: HsModule -> Maybe SrcLine
srcLineForNewImport module_ =
   case module_ of
        HS.Module srcSpan _ _ imports decls            -> newSrcLine srcSpan imports decls
        HS.XmlPage _ _ _ _ _ _ _                       -> Nothing
        HS.XmlHybrid srcSpan _ _ imports decls _ _ _ _ -> newSrcLine srcSpan imports decls
   where
      newSrcLine srcSpan imports decls
         | not $ null imports
         = Just (firstSrcLine $ last imports)

         | (decl:_) <- decls
         , sLoc <- declSrcLoc decl
         , HS.srcLine sLoc >= HS.startLine srcSpan
         = Just $ max 0 (HS.srcLine sLoc - 1)

         | otherwise
         = Nothing
