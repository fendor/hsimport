{-# Language PatternGuards #-}

module HsImport.Main
   ( hsImport
   ) where

import Control.Lens
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsImport.ImportChange
import HsImport.ImportSpec
import qualified HsImport.Parse as P
import HsImport.PrettyPrint (prettyPrint)


hsImport :: ImportSpec -> IO ()
hsImport spec = do
   let impChanges = importChanges (spec ^. moduleToImport)
                                  (spec ^. symbolToImport)
                                  (spec ^. qualifiedName)
                                  (spec ^. parsedSrcFile)

   srcLines <- lines . T.unpack <$> TIO.readFile (spec ^. sourceFile)
   let srcLines' = applyChanges srcLines impChanges
   when (srcLines' /= srcLines || isJust (spec ^. saveToFile)) $
      TIO.writeFile (outputFile spec) (T.pack $ unlines srcLines')

   where
      applyChanges = foldl' applyChange

      applyChange srcLines (ReplaceImportAt srcLine importDecl) =
         let numTakes = max 0 (srcLine - 1)
             numDrops = lastImportSrcLine srcLine srcLines
             in take numTakes srcLines ++ [prettyPrint importDecl] ++ drop numDrops srcLines

      applyChange srcLines (AddImportAfter srcLine importDecl) =
         let numTakes = srcLine
             numDrops = lastImportSrcLine srcLine srcLines
             in take numTakes srcLines ++ [prettyPrint importDecl] ++ drop numDrops srcLines

      applyChange srcLines (AddImportAtEnd importDecl) =
         srcLines ++ [prettyPrint importDecl]

      applyChange srcLines NoImportChange = srcLines

      outputFile spec
         | Just file <- spec ^. saveToFile = file
         | otherwise                       = spec ^. sourceFile

      lastImportSrcLine fstLine srcLines
         | Just lastLine <- P.lastImportSrcLine $ drop (max 0 (fstLine - 1)) srcLines
         = fstLine + (lastLine - 1)

         | otherwise
         = fstLine
