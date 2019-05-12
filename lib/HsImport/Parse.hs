{-# Language ScopedTypeVariables, PatternGuards, CPP #-}

module HsImport.Parse
   ( parseFile
   , replaceCpp
   ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)
import qualified Language.Haskell.Exts as HS
import Control.Exception (catch, SomeException)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import HsImport.Types

parseFile :: FilePath -> IO (Either Error ParseResult)
parseFile file = do
   srcFile <- replaceCpp . T.unpack <$> TIO.readFile file
   catch (do let result = parseFileContents srcFile
             case result of
                  HS.ParseOk _ -> return $ Right result

                  HS.ParseFailed srcLoc _ -> do
                     srcResult <- parseInvalidSource (lines srcFile) (HS.srcLine srcLoc)
                     return $ Right $ fromMaybe result srcResult)

         (\(e :: SomeException) -> do
            let srcLines = lines srcFile
            srcResult <- parseInvalidSource srcLines (length srcLines)
            return $ maybe (Left $ show e) Right srcResult)


-- | replace the complete cpp directive, from #ifdef till #endif, by empty lines
replaceCpp :: String -> String
replaceCpp contents = unlines . reverse $ go (lines contents) [] 0
   where
      go [] newLines _ = newLines
      go lines newLines ifdefLevel = go lines' newLines' ifdefLevel'
         where
            currLine    = head lines
            hasCpp      = "#" `isPrefixOf` currLine
            hasIf       = "#if" `isPrefixOf` currLine
            hasEndIf    = "#endif" `isPrefixOf` currLine
            ifdefLevel' = ifdefLevel + (if hasIf then 1 else 0) - (if hasEndIf then 1 else 0)
            lines'      = tail lines
            inCpp       = hasCpp || (max ifdefLevel ifdefLevel') > 0
            newLines'   = (if inCpp then "" else currLine) : newLines


-- | tries to find the maximal part of the source file (from the beginning) that contains
--   valid/complete Haskell code
parseInvalidSource :: [String] -> Int -> IO (Maybe ParseResult)
parseInvalidSource srcLines firstInvalidLine = do
   parseInvalidSource' 1 firstInvalidLine Nothing 0
   where
      parseInvalidSource' :: Int -> Int -> Maybe (Int, ParseResult) -> Int -> IO (Maybe ParseResult)
      parseInvalidSource' lastValidLine currLastLine maxParseOk iteration
         | null srcLines || lastValidLine >= currLastLine
         = return Nothing

         | iteration >= 10
         = return (snd <$> maxParseOk)

         | otherwise = do
            catch (case parseFileContents source of
                        result@(HS.ParseOk _)
                           | nextLine == currLastLine ->
                              return $ Just result
                           | otherwise                ->
                              parseInvalidSource' nextLine currLastLine (maxParseOk' result) iteration'

                        HS.ParseFailed srcLoc _
                           | HS.srcLine srcLoc == firstInvalidLine ->
                              parseInvalidSource' lastValidLine nextLine maxParseOk iteration'
                           | otherwise                             ->
                              parseInvalidSource' nextLine currLastLine maxParseOk iteration')

                  (\(_ :: SomeException) -> parseInvalidSource' lastValidLine nextLine maxParseOk iteration')
         where
            source      = unlines $ take nextLine srcLines
            nextLine    = lastValidLine + (floor ((realToFrac (currLastLine - lastValidLine) / 2) :: Double) :: Int)
            iteration'  = iteration + 1

            maxParseOk' nextResult =
               case maxParseOk of
                    Just (maxLine, _)
                       | nextLine > maxLine -> Just (nextLine, nextResult)
                       | otherwise          -> maxParseOk

                    _ -> Just (nextLine, nextResult)


parseFileContents :: String -> ParseResult
parseFileContents contents =
   let result = HS.parseFileContentsWithComments parseMode contents
       in case result of
               HS.ParseOk res          -> HS.ParseOk $ HS.associateHaddock res
               HS.ParseFailed sloc msg -> HS.ParseFailed sloc msg

   where
      parseMode = HS.defaultParseMode { HS.fixities = Just [] }

