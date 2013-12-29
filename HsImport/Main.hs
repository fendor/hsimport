{-# Language PatternGuards #-}

module HsImport.Main
   ( hsImport
   ) where

import Control.Lens
import System.Directory (copyFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import HsImport.ImportChange
import HsImport.ImportSpec


hsImport :: ImportSpec -> IO ()
hsImport spec =
   case importChange (spec ^. moduleToImport) (spec ^. symbolToImport) (spec ^. parsedSrcFile) of
        ReplaceImportAt srcLine importStr ->
           modifyLines $ \lines_ ->
              let numDrops   = srcLine
                  numTakes   = max 0 (numDrops - 1)
                  in take numTakes lines_ ++ [importStr] ++ drop numDrops lines_

        AddImportAfter srcLine importStr ->
           modifyLines $ \lines_ ->
              let numTakes   = srcLine
                  numDrops   = numTakes
                  in take numTakes lines_ ++ [importStr] ++ drop numDrops lines_

        AddImportAtEnd importStr -> modifyLines (++ [importStr])

        NoImportChange
           | Just saveTo <- spec ^. saveToFile -> copyFile (spec ^. sourceFile) saveTo
           | otherwise                         -> return ()
   where
      modifyLines f = do
         file <- TIO.readFile $ spec ^. sourceFile
         let lines_  = lines . T.unpack $ file
             lines_' = f lines_
             file'   = T.pack . unlines $ lines_'

         TIO.writeFile (outputFile spec) file'

         where
            outputFile spec
               | Just file <- spec ^. saveToFile = file
               | otherwise                       = spec ^. sourceFile
