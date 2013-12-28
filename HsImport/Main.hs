{-# Language PatternGuards #-}

module HsImport.Main
   ( hsImport
   ) where

import Control.Lens
import System.Directory (copyFile)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.Exts as HS
import HsImport.ImportChange
import HsImport.ImportSpec


hsImport :: ImportSpec -> IO ()
hsImport spec =
   case importChange (spec ^. moduleToImport) (spec ^. symbolToImport) (spec ^. parsedSrcFile) of
        ReplaceImport importDecl ->
           modifyLines $ \lines_ ->
              let importLine = HS.prettyPrint importDecl
                  numDrops   = HS.srcLine . HS.importLoc $ importDecl
                  numTakes   = max 0 (numDrops - 1)
                  in take numTakes lines_ ++ [importLine] ++ drop numDrops lines_

        AddImport importDecl ->
           modifyLines $ \lines_ ->
              let importLine = HS.prettyPrint importDecl
                  numTakes   = HS.srcLine . HS.importLoc $ importDecl
                  numDrops   = numTakes
                  in take numTakes lines_ ++ [importLine] ++ drop numDrops lines_

        AddImportAtEnd importDecl ->
           modifyLines $ \lines_ ->
              let importLine = HS.prettyPrint importDecl
                  in lines_ ++ [importLine]

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
