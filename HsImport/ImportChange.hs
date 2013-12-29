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

type SrcLine      = Int
type ImportString = String

data ImportChange = ReplaceImportAt SrcLine ImportString
                  | AddImportAfter SrcLine ImportString
                  | AddImportAtEnd ImportString
                  | NoImportChange
                  deriving (Show)


importChange :: String -> Maybe String -> HS.Module -> ImportChange
importChange moduleName (Just symbolName) module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching || any (symbolImported symbolName) matching
         then NoImportChange
         else case find hasImportedSymbols matching of
                   Just impDecl ->
                      ReplaceImportAt (srcLine impDecl) (HS.prettyPrint $ addSymbol impDecl symbolName)

                   Nothing      ->
                      AddImportAfter (srcLine . last $ matching)
                                     (HS.prettyPrint $ importDeclWithSymbol moduleName symbolName)

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImportAfter (srcLine bestMatch) (HS.prettyPrint $ importDeclWithSymbol moduleName symbolName)

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (HS.prettyPrint $ importDeclWithSymbol moduleName symbolName)
           Nothing      -> AddImportAtEnd (HS.prettyPrint $ importDeclWithSymbol moduleName symbolName)
   where
      addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbolName =
         id {HS.importSpecs = specs & _Just . _2 %~ (++ [HS.IVar $ hsName symbolName])}

importChange moduleName Nothing module_
   | matching@(_:_) <- matchingImports moduleName module_ =
      if any entireModuleImported matching
         then NoImportChange
         else AddImportAfter (srcLine . last $ matching) (HS.prettyPrint $ importDecl moduleName)

   | Just bestMatch <- bestMatchingImport moduleName module_ =
      AddImportAfter (srcLine bestMatch) (HS.prettyPrint $ importDecl moduleName)

   | otherwise =
      case srcLineForNewImport module_ of
           Just srcLine -> AddImportAfter srcLine (HS.prettyPrint $ importDecl moduleName)
           Nothing      -> AddImportAtEnd (HS.prettyPrint $ importDecl moduleName)


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
   | (HS.ImportDecl {HS.importSpecs = Just (False, (_:_))}) <- import_ = True
   | otherwise                                                         = False


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


importDeclWithSymbol :: String -> String -> HS.ImportDecl
importDeclWithSymbol moduleName symbolName =
   (importDecl moduleName) { HS.importSpecs = Just (False, [HS.IVar $ hsName symbolName]) }


hsName :: String -> HS.Name
hsName symbolName
   | isSymbol  = HS.Symbol symbolName
   | otherwise = HS.Ident symbolName
   where
      isSymbol = any (A.notInClass "a-zA-Z0-9_") symbolName


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
