{-# Language PatternGuards #-}

module HsImport.ImportChange
   ( ImportChange(..)
   , importChanges
   ) where

import Data.Maybe
import Data.List (find, (\\))
import Control.Monad (join)
import Lens.Micro
import qualified Language.Haskell.Exts as HS
import qualified Data.Attoparsec.Text as A
import HsImport.SymbolImport (SymbolImport(..), Symbol(..), symbol, isHiding, swapImport)
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
        then importQualifiedModule moduleName (fromMaybe moduleName as) hsModule
        else maybe NoImportChange (\asName -> importModuleAs moduleName asName hsModule) as
   ]


importModule :: String -> Module -> ImportChange
importModule moduleName module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any hasEntireModuleImported matching
         then NoImportChange
         else FindImportPos $ importDecl moduleName

   | not $ null (importDecls module_) =
      FindImportPos $ importDecl moduleName

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDecl moduleName)
           Nothing      -> AddImportAtEnd (importDecl moduleName)


existingMatching :: [ImportDecl] -> String -> SymbolImport -> ImportChange
existingMatching matching moduleName symbolImport
   | Just impDecl <- find hasEntireModuleImported matching =
      -- There is a module import
      if isHiding symbolImport
         then
            -- We add a hiding clause, since we only want to hide a very specific symbol.
            ReplaceImportAt (srcSpan . HS.ann $ impDecl) (setSymbol impDecl symbolImport)

         else
            -- If we want to import a symbol, we dont have to, since it is already imported.
            NoImportChange

   | any (hasSymbols symbolImport) matching =
      -- The symbol we want to import/hide is already imported/hidden.
      NoImportChange
   | otherwise =
      case find (hasAnySymbols $ isHiding symbolImport) matching of
         Just impDecl ->
            -- There is a fitting import declaration to which we can add the symbol to.
            ReplaceImportAt (srcSpan . HS.ann $ impDecl) (addSymbol impDecl symbolImport)

         Nothing      ->
            -- The symbol is either not imported/hidden or another import
            -- hides/imports it.
            -- If something explictly imports the symbol, we remove it from the import list.
            -- If something explictly hides the symbol, we remove it from the hiding list.
            case find (hasSymbols (swapImport symbolImport)) matching of
               Just impDecl ->
                  -- There is a import declaration that imports/hides the symbol we want to hide/import.
                  ReplaceImportAt (srcSpan . HS.ann $ impDecl) (removeSymbol impDecl symbolImport)

               Nothing ->
                  -- Module is not imported at all.
                  FindImportPos $ importDeclWithSymbol moduleName symbolImport

importModuleWithSymbol :: String -> SymbolImport -> Module -> ImportChange
importModuleWithSymbol moduleName symbolImport module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      -- There is already a matching import line
      existingMatching matching moduleName symbolImport

   | not $ null (importDecls module_) =
      FindImportPos $ importDeclWithSymbol moduleName symbolImport

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDeclWithSymbol moduleName symbolImport)
           Nothing      -> AddImportAtEnd (importDeclWithSymbol moduleName symbolImport)

-- | Extend the spec list with the given symbol.
-- Might result in duplciates.
extendSpecList :: SymbolImport -> HS.ImportSpecList Annotation -> HS.ImportSpecList Annotation
extendSpecList symbolImport (HS.ImportSpecList annotation hid specs) =
   HS.ImportSpecList annotation hid (specs ++ [importSpec $ symbol symbolImport])

-- | Remove an element from the import list if it matches the symbol.
-- If the resulting spec list is empty afterwards, Nothing is returned to remove
-- the import list.
removeSpecList :: SymbolImport -> HS.ImportSpecList Annotation -> Maybe (HS.ImportSpecList Annotation)
removeSpecList symbolImport (HS.ImportSpecList annotation hid specs) =
   let
      specListRemovedSymbol = filter (not . imports (symbol symbolImport)) specs
   in
   if null specListRemovedSymbol
      then Nothing -- Remove the spec list if it is empty now
      else Just $ HS.ImportSpecList annotation hid specListRemovedSymbol

-- | Set the spec list to the given symbol.
setSpecList :: SymbolImport -> Annotation -> HS.ImportSpecList Annotation
setSpecList symbolImport annotation =
   HS.ImportSpecList annotation (isHiding symbolImport) [importSpec $ symbol symbolImport]

-- | Add a symbol to the given spec list. May result in duplicates.
addSymbol :: ImportDecl -> SymbolImport -> ImportDecl
addSymbol id@HS.ImportDecl {HS.importSpecs = specs} symbolImport =
   id {HS.importSpecs = specs & _Just %~ extendSpecList symbolImport}

-- | Set a symbol to be exported from the current import declaration.
-- Does not care whether the import declaration already has a spec list.
setSymbol :: ImportDecl -> SymbolImport -> ImportDecl
setSymbol id@HS.ImportDecl {HS.importAnn = importAnn } symbolImport =
   id {HS.importSpecs = Just (setSpecList symbolImport importAnn) }

-- | Remove a symbol from the import declaration.
-- May remove the whole spec list if the list is empty after removal.
removeSymbol :: ImportDecl -> SymbolImport -> ImportDecl
removeSymbol id@HS.ImportDecl {HS.importSpecs = specs} symbolImport =
   id {HS.importSpecs = join (specs & _Just %~ removeSpecList symbolImport) }


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


hasEntireModuleImported :: ImportDecl -> Bool
hasEntireModuleImported import_ =
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

-- | Checks whether the given SymbolImport is already covered by the current ImportDecl.
hasSymbols :: SymbolImport -> ImportDecl -> Bool
hasSymbols symbolImport import_
   | Just (HS.ImportSpecList _ hidden hsSymbols) <- HS.importSpecs import_
   , hidden == isHiding symbolImport
   , any (imports $ symbol symbolImport) hsSymbols
   = True

   | otherwise
   = False

-- | Checks whether the given symbol is mentioned by the import spec.
imports :: Symbol -> ImportSpec -> Bool
imports = imports_
   where
      imports_ :: Symbol -> ImportSpec -> Bool
      imports_ (Only symName)         (HS.IVar _ name)                    = symName == nameString name
      imports_ (Only symName)         (HS.IAbs _ _ name)                  = symName == nameString name
      imports_ (Only symName)         (HS.IThingAll _ name)               = symName == nameString name
      imports_ (Only symName)         (HS.IThingWith _ name _)            = symName == nameString name
      imports_ (AllOf symName)        (HS.IThingAll _ name)               = symName == nameString name
      imports_ (SomeOf symName _    ) (HS.IThingAll _ name)               = symName == nameString name
      imports_ (SomeOf symName names) (HS.IThingWith _ hsSymName hsNames) =
         symName == nameString hsSymName && null (names \\ (map (nameString . toName) hsNames))

      imports_ _ _ = False
      nameString (HS.Ident _ id)   = id
      nameString (HS.Symbol _ sym) = sym

      toName (HS.VarName _ name) = name
      toName (HS.ConName _ name) = name


hasAnySymbols :: Bool -> ImportDecl -> Bool
hasAnySymbols hiding import_
   | Just (HS.ImportSpecList _ hid (_:_)) <- HS.importSpecs import_
   , hid == hiding
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
   case symbolImport of
      Hiding s -> makeImportDecl True s
      Import s -> makeImportDecl False s
   where
      makeImportDecl :: Bool -> Symbol -> ImportDecl
      makeImportDecl hiding symbols =
         (importDecl moduleName)
            { HS.importSpecs = Just (HS.ImportSpecList noAnnotation
                                                       hiding
                                                       [importSpec symbols])
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


importSpec :: Symbol -> ImportSpec
importSpec (Only symName)         = HS.IVar noAnnotation (hsName symName)
importSpec (AllOf symName)        = HS.IThingAll noAnnotation (hsName symName)
importSpec (SomeOf symName names) = HS.IThingWith noAnnotation
                                                  (hsName symName)
                                                  (map (HS.VarName noAnnotation . hsName) names)

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
