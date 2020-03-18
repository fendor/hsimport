{-# Language ScopedTypeVariables, PatternGuards, CPP #-}

module HsImport.Parse
   ( parseFile
   , replaceCpp
   ) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.List (isPrefixOf)
import DynFlags (DynFlags)
import qualified Parser as HS
import qualified Lexer as HS
import qualified SrcLoc as HS
import qualified StringBuffer
import qualified FastString
import Control.Exception (catch, SomeException)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import HsImport.Types

parseFile :: FilePath -> IO (Either ErrorMessage ParseResult)
parseFile file = do
   -- srcFile <- replaceCpp . T.unpack <$> TIO.readFile file
   srcFile <- T.unpack <$> TIO.readFile file
   catch (case parseFileContents undefined srcFile of
                  result@(HS.POk _ _) -> return . Right $ ParseResult result Nothing

                  HS.PFailed _ _ _ -> return $ Left ""
                  -- HS.PFailed _ srcSpan _ -> do
                     -- srcResult <- parseInvalidSource (lines srcFile) (HS.srcLine srcLoc)
                     -- return $ case srcResult of
                     --               Just srcRes -> Right srcRes
                     --               Nothing -> Right $ ParseResult result Nothing)
         )
         (\(e :: SomeException) -> do
            return (Left $ show e)
            -- let srcLines = lines srcFile
            -- srcResult <- parseInvalidSource srcLines (length srcLines)
            -- return $ maybe (Left $ show e) Right srcResult)
         )


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


type HsParseResult = HS.ParseResult (HS.Located Module)

-- -- | tries to find the maximal part of the source file (from the beginning) that contains
-- --   valid/complete Haskell code
-- parseInvalidSource :: [String] -> Int -> IO (Maybe ParseResult)
-- parseInvalidSource srcLines firstInvalidLine = do
--    parseInvalidSource' 1 firstInvalidLine Nothing 0
--    where
--       parseInvalidSource' :: Int -> Int -> Maybe (Int, HsParseResult) -> Int -> IO (Maybe ParseResult)
--       parseInvalidSource' lastValidLine currLastLine maxParseOk iteration
--          | null srcLines || lastValidLine >= currLastLine
--          = return Nothing

--          | iteration >= 10
--          = return $ case maxParseOk of
--                          Just (lastLine, result) -> Just $ ParseResult result (Just lastLine)
--                          Nothing -> Nothing

--          | otherwise = do
--             catch (case parseFileContents source of
--                         result@(HS.ParseOk _)
--                            | nextLine == currLastLine -> return . Just $ ParseResult result (Just lastValidLine)
--                            | otherwise -> parseInvalidSource' nextLine currLastLine (maxParseOk' result) iteration'

--                         HS.ParseFailed srcLoc _
--                            | HS.srcLine srcLoc == firstInvalidLine ->
--                               parseInvalidSource' lastValidLine nextLine maxParseOk iteration'

--                            | otherwise ->
--                               parseInvalidSource' nextLine currLastLine maxParseOk iteration')

--                   (\(_ :: SomeException) -> parseInvalidSource' lastValidLine nextLine maxParseOk iteration')
--          where
--             source      = unlines $ take nextLine srcLines
--             nextLine    = lastValidLine + (floor ((realToFrac (currLastLine - lastValidLine) / 2) :: Double) :: Int)
--             iteration'  = iteration + 1

--             maxParseOk' nextResult =
--                case maxParseOk of
--                     Just (maxLine, _)
--                        | nextLine > maxLine -> Just (nextLine, nextResult)
--                        | otherwise          -> maxParseOk

--                     _ -> Just (nextLine, nextResult)


parseFileContents :: DynFlags -> String -> HsParseResult
parseFileContents flags contents = HS.unP HS.parseModule parseState
   where
      filename = "<interactive>"
      location = HS.mkRealSrcLoc (FastString.mkFastString filename) 1 1
      buffer = StringBuffer.stringToStringBuffer contents
      parseState = HS.mkPState flags buffer location

