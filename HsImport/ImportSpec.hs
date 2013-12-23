{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsImport.ImportSpec
   ( ImportSpec(..)
   , sourceFile
   , parsedSrcFile
   , moduleToImport
   , symbolToImport
   , hsImportSpec
   ) where

import Control.Lens
import Control.Monad.State
import qualified Language.Haskell.Exts as HS
import qualified HsImport.Args as Args
import HsImport.Args (HsImportArgs)

data ImportSpec = ImportSpec {
  _sourceFile     :: FilePath,
  _parsedSrcFile  :: HS.Module,
  _moduleToImport :: String,
  _symbolToImport :: Maybe String
  } deriving (Show)  

makeLenses ''ImportSpec


type Error = String
hsImportSpec :: HsImportArgs -> IO (Either Error ImportSpec)
hsImportSpec args
   | Just error <- checkArgs args = return $ Left error
   | otherwise = do
      result <- HS.parseFile $ Args.sourceFile args
      case result of
           HS.ParseOk modul -> return $ Right $
              ImportSpec (Args.sourceFile args) modul
                         (Args.moduleName args) symbolName

           HS.ParseFailed _ error -> return $ Left error

   where
      symbolName =
         case Args.symbolName args of
              ""  -> Nothing
              sym -> Just sym

      checkArgs args
         | null . Args.sourceFile $ args = Just "Missing source file!"
         | null . Args.moduleName $ args = Just "Missing module name!"
         | otherwise                     = Nothing
