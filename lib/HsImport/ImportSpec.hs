{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsImport.ImportSpec
   ( ImportSpec(..)
   , hsImportSpec
   ) where

import qualified Language.Haskell.Exts as HS
import qualified HsImport.Args as Args
import HsImport.Args (HsImportArgs)
import HsImport.Parse (parseFile)
import HsImport.Symbol (Symbol(..))
import HsImport.Module
import Data.List (find)

data ImportSpec = ImportSpec
   { sourceFile     :: FilePath
   , parsedSrcFile  :: HS.Module
   , moduleToImport :: Module
   , symbolToImport :: Maybe Symbol
   , saveToFile     :: Maybe FilePath
   } deriving (Show)


type Error = String
hsImportSpec :: HsImportArgs -> IO (Either Error ImportSpec)
hsImportSpec args
   | Just error <- checkArgs args = return $ Left error
   | otherwise = do
      result <- parseFile $ Args.inputSrcFile args
      case result of
           Right (HS.ParseOk hsModule) -> return $ Right $
              ImportSpec { sourceFile    = Args.inputSrcFile args
                         , parsedSrcFile = hsModule
                         , moduleToImport = module_
                         , symbolToImport = symbol
                         , saveToFile     = saveToFile
                         }

           Right (HS.ParseFailed srcLoc error) -> return $ Left (show srcLoc ++ error)

           Left error -> return $ Left error

   where
      module_ = Module { moduleName = Args.moduleName args
                       , qualified  = not . null $ Args.qualifiedName args
                       , as         = find (/= "") [Args.qualifiedName args, Args.as args]
                       }

      symbol =
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
