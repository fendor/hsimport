{-# LANGUAGE TemplateHaskell, PatternGuards #-}

module HsImport.ImportSpec
   ( ImportSpec(..)
   , sourceFile
   , parsedSrcFile
   , moduleToImport
   , symbolToImport
   , qualifiedName
   , saveToFile
   , hsImportSpec
   ) where

import Control.Lens
import qualified Language.Haskell.Exts as HS
import qualified HsImport.Args as Args
import HsImport.Args (HsImportArgs)
import HsImport.Parse (parseFile)
import HsImport.Symbol (Symbol(..))

data ImportSpec = ImportSpec 
   { _sourceFile       :: FilePath
   , _parsedSrcFile    :: HS.Module
   , _moduleToImport   :: String
   , _symbolToImport   :: Maybe Symbol
   , _qualifiedName    :: Maybe String
   , _saveToFile       :: Maybe FilePath
   } deriving (Show)  


makeLenses ''ImportSpec


type Error = String
hsImportSpec :: HsImportArgs -> IO (Either Error ImportSpec)
hsImportSpec args
   | Just error <- checkArgs args = return $ Left error
   | otherwise = do
      result <- parseFile $ Args.inputSrcFile args
      case result of
           Right (HS.ParseOk modul) -> return $ Right $
              ImportSpec (Args.inputSrcFile args) modul
                         (Args.moduleName args) symbol
                         qualifiedName saveToFile

           Right (HS.ParseFailed srcLoc error) -> return $ Left (show srcLoc ++ error)

           Left error -> return $ Left error

   where
      symbol =
         case Args.symbolName args of
              ""  -> Nothing

              name | Args.all args              -> Just $ AllOfSymbol name
                   | ws@(_:_) <- Args.with args -> Just $ SomeOfSymbol name ws
                   | otherwise                  -> Just $ Symbol name

      qualifiedName =
         case Args.qualifiedName args of
              "" -> Nothing
              qn -> Just qn

      saveToFile =
         case Args.outputSrcFile args of
              "" -> Nothing
              fp -> Just fp

      checkArgs args
         | null . Args.inputSrcFile $ args = Just "Missing source file!"
         | null . Args.moduleName $ args   = Just "Missing module name!"
         | otherwise                       = Nothing
