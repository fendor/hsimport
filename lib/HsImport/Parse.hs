{-# Language ScopedTypeVariables, PatternGuards, CPP #-}

module HsImport.Parse
   ( parseFile
   , lastImportSrcLine
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

type Error         = String
type HsParseResult = HS.ParseResult (HS.Module HS.SrcSpanInfo)

parseFile :: FilePath -> IO (Either Error HsParseResult)
parseFile file = do
   srcFile <- unlines. replaceCPPByComment . lines . T.unpack <$> TIO.readFile file
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
   where
      -- | replace CPP directives by a fake comment
      replaceCPPByComment = map $ \line ->
         if "#" `isPrefixOf` line
            then "-- fake hsimport comment"
            else line


type SrcLine = Int

-- | Expects that '[String]' starts with an import declaration and returns
--   the last source line of the import declaration, so this function
--   is for the handling of multine line import declarations.
lastImportSrcLine :: [String] -> Maybe SrcLine
lastImportSrcLine srcLines
   | null srcLines
   = Nothing

   | "import" `isPrefixOf` head srcLines
   = parseImport 1

   | otherwise
   = Nothing

   where
      parseImport lastLine
         | lastLine <= numSrcLines
         = case parseFileContents source of
                HS.ParseOk module_
                   | oneImportDeclWithoutSymbols module_ && startsWithImportDeclSymbols (lastLine + 1)
                     -> parseImport (lastLine + 1)

                   | otherwise
                     -> Just lastLine

                HS.ParseFailed _ _ -> parseImport (lastLine + 1)

         | otherwise
         = Nothing

         where
            source = unlines $ take lastLine srcLines

      numSrcLines = length srcLines

      -- | Returns True if the module contains one ImportDecl without any explicitely
      --   listed symbols.
      oneImportDeclWithoutSymbols (HS.Module _ _ _ [HS.ImportDecl {HS.importSpecs = Nothing}] _) = True
      oneImportDeclWithoutSymbols _                                                              = False

      -- | Returns True if the line represents the starting of a ImportDecl symbol list.
      startsWithImportDeclSymbols lineNum
         | line : _ <- drop (lineNum - 1) srcLines
         , ' '  : _ <- line
         , '('  : _ <- dropWhile (== ' ') line
         = True

         | otherwise
         = False


-- | tries to find the maximal part of the source file (from the beginning) that contains
--   valid/complete Haskell code
parseInvalidSource :: [String] -> Int -> IO (Maybe HsParseResult)
parseInvalidSource srcLines firstInvalidLine = do
   parseInvalidSource' 1 firstInvalidLine Nothing 0
   where
      parseInvalidSource' :: Int -> Int -> Maybe (Int, HsParseResult) -> Int -> IO (Maybe HsParseResult)
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


parseFileContents :: String -> HsParseResult
parseFileContents = HS.parseFileContentsWithMode parseMode
   where
      parseMode = HS.defaultParseMode { HS.fixities = Just [] }
