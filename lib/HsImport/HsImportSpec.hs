{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsImport.HsImportSpec
   ( HsImportSpec(..)
   , hsImportSpec
   ) where

import qualified Language.Haskell.Exts as HS
import qualified HsImport.Args as Args
import HsImport.Args (HsImportArgs)
import HsImport.Parse (parseFile)
import HsImport.SymbolImport (SymbolImport(..))
import HsImport.ModuleImport
import HsImport.Types
import Data.List (find)

data HsImportSpec = HsImportSpec
   { sourceFile    :: FilePath
   , parsedSrcFile :: Module
   , moduleImport  :: ModuleImport
   , symbolImport  :: Maybe SymbolImport
   , saveToFile    :: Maybe FilePath
   } deriving (Show)


hsImportSpec :: HsImportArgs -> IO (Either Error HsImportSpec)
hsImportSpec args
   | Just error <- checkArgs args = return $ Left error
   | otherwise = do
      result <- parseFile $ Args.inputSrcFile args
      case result of
           Right (ParseResult (HS.ParseOk hsModule) _) -> return $ Right $
              HsImportSpec { sourceFile    = Args.inputSrcFile args
                           , parsedSrcFile = hsModule
                           , moduleImport  = module_
                           , symbolImport  = symbolImport
                           , saveToFile    = saveToFile
                           }

           Right (ParseResult (HS.ParseFailed srcLoc error) _) -> return $ Left (show srcLoc ++ error)

           Left error -> return $ Left error

   where
      module_ = ModuleImport { moduleName = Args.moduleName args
                             , qualified  = not . null $ Args.qualifiedName args
                             , as         = find (/= "") [Args.qualifiedName args, Args.as args]
                             }

      symbolImport =
         case Args.symbolName args of
              ""  -> Nothing

              name | Args.all args              -> Just $ AllOfSymbol name
                   | ws@(_:_) <- Args.with args -> Just $ SomeOfSymbol name ws
                   | otherwise                  -> Just $ Symbol name

      saveToFile =
         case Args.outputSrcFile args of
              "" -> Nothing
              fp -> Just fp

      checkArgs args
         | null . Args.inputSrcFile $ args
         = Just "Missing source file!"

         | null . Args.moduleName $ args
         = Just "Missing module name!"

         | (not . null $ qualifiedName) && (not . null $ asName)
         = Just "Invalid usage of options '--qualifiedname' and '--as' at once!"

         | otherwise
         = Nothing
         where
            qualifiedName = Args.qualifiedName args
            asName        = Args.as args
