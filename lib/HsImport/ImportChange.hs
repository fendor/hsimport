{-# Language PatternGuards #-}

module HsImport.ImportChange
   ( ImportChange(..)
   , importChanges
   ) where

import Data.Maybe
import Data.List (find, (\\))
import Control.Lens
import qualified Language.Haskell.Exts as HS
import qualified Data.Attoparsec.Text as A
import HsImport.Symbol (Symbol(..))
import HsImport.Module (Module(..))
import HsImport.ImportPos (matchingImports)
import HsImport.Utils

type SrcLine = Int

-- | How the import declarations should be changed
data ImportChange = ReplaceImportAt SrcLine HS.ImportDecl -- ^ replace the import declaration at SrcLine
                  | AddImportAfter SrcLine HS.ImportDecl  -- ^ add import declaration after SrcLine
                  | AddImportAtEnd HS.ImportDecl          -- ^ add import declaration at end of source file
                  | FindImportPos HS.ImportDecl           -- ^ search for an insert position for the import declaration
                  | NoImportChange                        -- ^ no changes of the import declarations
                  deriving (Show)


importChanges :: Module -> Maybe Symbol -> HS.Module -> [ImportChange]
importChanges (Module moduleName False Nothing) Nothing hsModule =
   [ importModule moduleName hsModule ]

importChanges (Module moduleName False Nothing) (Just symbol) hsModule =
   [ importModuleWithSymbol moduleName symbol hsModule ]

importChanges (Module moduleName qualified as) symbol hsModule =
   [ maybe NoImportChange (\sym -> importModuleWithSymbol moduleName sym hsModule) symbol

   , if qualified
        then importQualifiedModule moduleName (maybe moduleName id as) hsModule
        else maybe NoImportChange (\asName -> importModuleAs moduleName asName hsModule) as
   ]


importModule :: String -> HS.Module -> ImportChange
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


importModuleWithSymbol :: String -> Symbol -> HS.Module -> ImportChange
importModuleWithSymbol moduleName symbol module_
   | matching@(_:_) <- matchingImports moduleName (importDecls module_) =
      if any entireModuleImported matching || any (symbolImported symbol) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl ->
                      ReplaceImportAt (srcLine impDecl) (addSymbol impDecl symbol)

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
         id {HS.importSpecs = specs & _Just . _2 %~ (++ [importSpec symbol])}


importQualifiedModule :: String -> String -> HS.Module -> ImportChange
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


importModuleAs :: String -> String -> HS.Module -> ImportChange
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


entireModuleImported :: HS.ImportDecl -> Bool
entireModuleImported import_ =
   not (HS.importQualified import_) && isNothing (HS.importSpecs import_)


hasQualifiedImport :: String -> HS.ImportDecl -> Bool
hasQualifiedImport qualifiedName import_
   | HS.importQualified import_
   , Just (HS.ModuleName importAs) <- HS.importAs import_
   , importAs == qualifiedName
   = True

   | otherwise = False


hasAsImport :: String -> HS.ImportDecl -> Bool
hasAsImport asName import_
   | not $ HS.importQualified import_
   , Just (HS.ModuleName importAs) <- HS.importAs import_
   , importAs == asName
   = True

   | otherwise
   = False


symbolImported :: Symbol -> HS.ImportDecl -> Bool
symbolImported symbol import_
   | Just (False, hsSymbols) <- HS.importSpecs import_
   , any (imports symbol) hsSymbols
   = True

   | otherwise
   = False
   where
      imports (Symbol symName)             (HS.IVar _ name)                  = symName == nameString name
      imports (Symbol symName)             (HS.IAbs name)                    = symName == nameString name
      imports (Symbol symName)             (HS.IThingAll name)               = symName == nameString name
      imports (Symbol symName)             (HS.IThingWith name _)            = symName == nameString name
      imports (AllOfSymbol symName)        (HS.IThingAll name)               = symName == nameString name
      imports (SomeOfSymbol symName _    ) (HS.IThingAll name)               = symName == nameString name
      imports (SomeOfSymbol symName names) (HS.IThingWith hsSymName hsNames) =
         symName == nameString hsSymName && null (names \\ (map (nameString . toName) hsNames))

      imports _ _ = False

      nameString (HS.Ident  id)  = id
      nameString (HS.Symbol sym) = sym

      toName (HS.VarName name) = name
      toName (HS.ConName name) = name


hasImportedSymbols :: HS.ImportDecl -> Bool
hasImportedSymbols import_
   | Just (False, _:_) <- HS.importSpecs import_ = True
   | otherwise                                   = False


importDecl :: String -> HS.ImportDecl
importDecl moduleName = HS.ImportDecl
   { HS.importLoc       = HS.SrcLoc "" 0 0
   , HS.importModule    = HS.ModuleName moduleName
   , HS.importQualified = False
   , HS.importSrc       = False
   , HS.importSafe      = False
   , HS.importPkg       = Nothing
   , HS.importAs        = Nothing
   , HS.importSpecs     = Nothing
   }


importDeclWithSymbol :: String -> Symbol -> HS.ImportDecl
importDeclWithSymbol moduleName symbol =
   (importDecl moduleName) { HS.importSpecs = Just (False, [importSpec symbol]) }


qualifiedImportDecl :: String -> String -> HS.ImportDecl
qualifiedImportDecl moduleName qualifiedName =
   (importDecl moduleName) { HS.importQualified = True
                           , HS.importAs        = if moduleName /= qualifiedName
                                                     then Just $ HS.ModuleName qualifiedName
                                                     else Nothing
                           }


asImportDecl :: String -> String -> HS.ImportDecl
asImportDecl moduleName asName =
   (importDecl moduleName) { HS.importQualified = False
                           , HS.importAs        = Just $ HS.ModuleName asName
                           }


importSpec :: Symbol -> HS.ImportSpec
importSpec (Symbol symName)             = HS.IVar HS.NoNamespace (hsName symName)
importSpec (AllOfSymbol symName)        = HS.IThingAll $ hsName symName
importSpec (SomeOfSymbol symName names) = HS.IThingWith (hsName symName) (map (HS.VarName . hsName) names)


hsName :: String -> HS.Name
hsName symbolName
   | isSymbol  = HS.Symbol symbolName
   | otherwise = HS.Ident symbolName
   where
      isSymbol = any (A.notInClass "a-zA-Z0-9_'") symbolName


srcLineForNewImport :: HS.Module -> Maybe SrcLine
srcLineForNewImport (HS.Module modSrcLoc _ _ _ _ imports decls)
   | not $ null imports = Just (srcLine $ last imports)

   | (decl:_)  <- decls
   , Just sLoc <- declSrcLoc decl
   , HS.srcLine sLoc >= HS.srcLine modSrcLoc
   = Just $ max 0 (HS.srcLine sLoc - 1)

   | otherwise = Nothing
