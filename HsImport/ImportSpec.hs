{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsImport.ImportSpec
   ( ImportSpec(..)
   , sourceFile
   , parsedSrcFile
   , moduleToImport
   , symbolToImport
   , saveToFile
   , hsImportSpec
   ) where

import Control.Lens
import qualified Language.Haskell.Exts as HS
import qualified HsImport.Args as Args
import HsImport.Args (HsImportArgs)
import HsImport.Parse (parseFile)

data ImportSpec = ImportSpec 
   { _sourceFile     :: FilePath
   , _parsedSrcFile  :: HS.Module
   , _moduleToImport :: String
   , _symbolToImport :: Maybe String
   , _saveToFile     :: Maybe FilePath
   } deriving (Show)  

makeLenses ''ImportSpec


type Error = String
hsImportSpec :: HsImportArgs -> IO (Either Error ImportSpec)
hsImportSpec args
   | Just error <- checkArgs args = return $ Left error
   | otherwise = do
      result <- parseFile $ Args.inputSrcFile args
      case result of
           HS.ParseOk modul -> return $ Right $
              ImportSpec (Args.inputSrcFile args) modul
                         (Args.moduleName args) symbolName saveToFile

           HS.ParseFailed _ error -> return $ Left error

   where
      symbolName =
         case Args.symbolName args of
              ""  -> Nothing
              sym -> Just sym

      saveToFile =
         case Args.outputSrcFile args of
              "" -> Nothing
              fp -> Just fp

      checkArgs args
         | null . Args.inputSrcFile $ args = Just "Missing source file!"
         | null . Args.moduleName $ args   = Just "Missing module name!"
         | otherwise                       = Nothing
