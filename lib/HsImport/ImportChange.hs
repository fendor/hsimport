{-# Language PatternGuards #-}

module HsImport.ImportChange
   ( ImportChange(..)
   , importChanges
   ) where

import Data.Maybe
import Data.List (find, (\\))
import Data.List.Split (splitOn)
import Control.Lens
import qualified Language.Haskell.Exts as HS
import qualified Data.Attoparsec.Text as A
import HsImport.Symbol (Symbol(..))

type SrcLine = Int

data ImportChange = ReplaceImportAt SrcLine HS.ImportDecl
                  | AddImportAfter SrcLine HS.ImportDecl
                  | AddImportAtEnd HS.ImportDecl
                  | NoImportChange
                  deriving (Show)


importChanges :: String -> Maybe Symbol -> Maybe String -> HS.Module -> [ImportChange]
importChanges moduleName (Just symbol) (Just qualifiedName) module_ =
   [ importModuleWithSymbol moduleName symbol module_
   , importQualifiedModule moduleName qualifiedName module_
   ]

importChanges moduleName (Just symbol) Nothing module_ =
   [ importModuleWithSymbol moduleName symbol module_ ]

importChanges moduleName Nothing (Just qualifiedName) module_ =
   [ importQualifiedModule moduleName qualifiedName module_ ]

importChanges moduleName Nothing Nothing module_ =
   [ importModule moduleName module_ ]


importModule :: String -> HS.Module -> ImportChange
importModule moduleName module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching
         then NoImportChange
         else AddImportAfter (srcLine . last $ matching) (importDecl moduleName)

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImportAfter (srcLine bestMatch) (importDecl moduleName)

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDecl moduleName)
           Nothing      -> AddImportAtEnd (importDecl moduleName)


importModuleWithSymbol :: String -> Symbol -> HS.Module -> ImportChange
importModuleWithSymbol moduleName symbol module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching || any (symbolImported symbol) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl ->
                      ReplaceImportAt (srcLine impDecl) (addSymbol impDecl symbol)

                   Nothing      ->
                      AddImportAfter (srcLine . last $ matching)
                                     (importDeclWithSymbol moduleName symbol)

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImportAfter (srcLine bestMatch) (importDeclWithSymbol moduleName symbol)

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (importDeclWithSymbol moduleName symbol)
           Nothing      -> AddImportAtEnd (importDeclWithSymbol moduleName symbol)
   where
      addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbol =
         id {HS.importSpecs = specs & _Just . _2 %~ (++ [importSpec symbol])}


importQualifiedModule :: String -> String -> HS.Module -> ImportChange
importQualifiedModule moduleName qualifiedName module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any (hasQualifiedImport qualifiedName) matching
         then NoImportChange
         else AddImportAfter (srcLine . last $ matching) (qualifiedImportDecl moduleName qualifiedName)

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImportAfter (srcLine bestMatch) (qualifiedImportDecl moduleName qualifiedName)

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (qualifiedImportDecl moduleName qualifiedName)
           Nothing      -> AddImportAtEnd (qualifiedImportDecl moduleName qualifiedName)


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
      computeMatches :: Int -> Maybe (Int, Int) -> [String] -> Maybe (Int, Int)
      computeMatches idx matches mod =
         let num' = numMatches splittedMod mod
             in case matches of
                     Just (_, num) | num' >= num -> Just (idx, num')
                                   | otherwise   -> matches

                     Nothing | num' > 0  -> Just (idx, num')
                             | otherwise -> Nothing
         where
            numMatches = loop 0
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


symbolImported :: Symbol -> HS.ImportDecl -> Bool
symbolImported symbol import_
   | Just (False, hsSymbols) <- HS.importSpecs import_
   , any (imports symbol) hsSymbols
   = True

   | otherwise
   = False
   where
      imports (Symbol symName)             (HS.IVar name)                    = symName == nameString name
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


importSpec :: Symbol -> HS.ImportSpec
importSpec (Symbol symName)             = HS.IVar $ hsName symName
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


srcLine :: HS.ImportDecl -> SrcLine
srcLine = HS.srcLine . HS.importLoc


declSrcLoc :: HS.Decl -> Maybe HS.SrcLoc
declSrcLoc decl =
   case decl of
        HS.TypeDecl srcLoc _ _ _          -> Just srcLoc
        HS.TypeFamDecl srcLoc _ _ _       -> Just srcLoc
        HS.DataDecl srcLoc _ _ _ _ _ _    -> Just srcLoc
        HS.GDataDecl srcLoc _ _ _ _ _ _ _ -> Just srcLoc
        HS.DataFamDecl srcLoc _ _ _ _     -> Just srcLoc
        HS.TypeInsDecl srcLoc _ _         -> Just srcLoc
        HS.DataInsDecl srcLoc _ _ _ _     -> Just srcLoc
        HS.GDataInsDecl srcLoc _ _ _ _ _  -> Just srcLoc
        HS.ClassDecl srcLoc _ _ _ _ _     -> Just srcLoc
        HS.InstDecl srcLoc _ _ _ _        -> Just srcLoc
        HS.DerivDecl srcLoc _ _ _         -> Just srcLoc
        HS.InfixDecl srcLoc _ _ _         -> Just srcLoc
        HS.DefaultDecl srcLoc _           -> Just srcLoc
        HS.SpliceDecl srcLoc _            -> Just srcLoc
        HS.TypeSig srcLoc _ _             -> Just srcLoc
        HS.FunBind _                      -> Nothing
        HS.PatBind srcLoc _ _ _ _         -> Just srcLoc
        HS.ForImp srcLoc _ _ _ _ _        -> Just srcLoc
        HS.ForExp srcLoc _ _ _ _          -> Just srcLoc
        HS.RulePragmaDecl srcLoc _        -> Just srcLoc
        HS.DeprPragmaDecl srcLoc _        -> Just srcLoc
        HS.WarnPragmaDecl srcLoc _        -> Just srcLoc
        HS.InlineSig srcLoc _ _ _         -> Just srcLoc
        HS.InlineConlikeSig srcLoc _ _    -> Just srcLoc
        HS.SpecSig srcLoc _ _ _           -> Just srcLoc
        HS.SpecInlineSig srcLoc _ _ _ _   -> Just srcLoc
        HS.InstSig srcLoc _ _ _           -> Just srcLoc
        HS.AnnPragma srcLoc _             -> Just srcLoc
