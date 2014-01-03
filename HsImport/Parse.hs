{-# Language ScopedTypeVariables #-}

module HsImport.Parse
   ( parseFile
   ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.Exts as HS
import Control.Applicative ((<$>))
import Control.Exception (catch, SomeException)

type Error = String

parseFile :: FilePath -> IO (Either Error (HS.ParseResult HS.Module))
parseFile file = do
   srcFile <- T.unpack <$> TIO.readFile file
   catch (do let result = HS.parseFileContents srcFile
             case result of
                  HS.ParseOk _ -> return $ Right result

                  HS.ParseFailed srcLoc _ -> do
                     srcResult <- parseInvalidSource (lines srcFile) 0 (HS.srcLine srcLoc)
                     return $ Right $ fromMaybe result srcResult)

         (\(e :: SomeException) -> do
            let srcLines = lines srcFile
            srcResult <- parseInvalidSource srcLines 0 (length srcLines)
            return $ maybe (Left $ show e) Right srcResult)

-- | tries to find the maximal part of the source file (from the beginning) that contains
--   valid/complete Haskell code
parseInvalidSource :: [String] -> Int -> Int -> IO (Maybe (HS.ParseResult HS.Module))
parseInvalidSource srcLines lastValidLine firstInvalidLine
   | null srcLines || lastValidLine >= firstInvalidLine = return Nothing
   | otherwise =
      catch (case HS.parseFileContents source of
                  result@(HS.ParseOk _)
                     | (nextLine + 1) == firstInvalidLine ->
                        return $ Just result
                     | otherwise                          ->
                        parseInvalidSource srcLines nextLine firstInvalidLine

                  HS.ParseFailed _ _ -> parseInvalidSource srcLines lastValidLine nextLine)

            (\(_ :: SomeException) -> parseInvalidSource srcLines lastValidLine nextLine)
   where
      source   = unlines $ take (nextLine + 1) srcLines
      nextLine = lastValidLine + (floor ((realToFrac (firstInvalidLine - lastValidLine) / 2) :: Double) :: Int)
