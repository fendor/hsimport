
module HsImport.Parse
   ( parseFile
   ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Language.Haskell.Exts as HS
import Control.Applicative ((<$>))


parseFile :: FilePath -> IO (HS.ParseResult HS.Module)
parseFile file = do
   result <- HS.parseFile file
   case result of
        HS.ParseOk _ -> return result

        HS.ParseFailed srcLoc _ -> do
           srcLines <- lines . T.unpack <$> TIO.readFile file
           case parseInvalidSource srcLines 0 (HS.srcLine srcLoc) of
                Just br -> return br
                _       -> return result 


-- | tries to find the maximal part of the source file (from the beginning) that contains
--   valid/complete Haskell code
parseInvalidSource :: [String] -> Int -> Int -> Maybe (HS.ParseResult HS.Module)
parseInvalidSource srcLines lastValidLine firstInvalidLine
   | null srcLines || lastValidLine >= firstInvalidLine = Nothing
   | otherwise =
      case HS.parseFileContents source of
           result@(HS.ParseOk _) 
              | (nextLine + 1) == firstInvalidLine -> 
                 Just result
              | otherwise                          -> 
                 parseInvalidSource srcLines nextLine firstInvalidLine

           HS.ParseFailed _ _ -> parseInvalidSource srcLines lastValidLine nextLine 
   where
      source   = unlines $ take (nextLine + 1) srcLines
      nextLine = lastValidLine + (floor ((realToFrac (firstInvalidLine - lastValidLine) / 2) :: Double) :: Int)
