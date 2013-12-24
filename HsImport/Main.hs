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
        ReplaceImport importDecl -> do
           let numDrops = HS.srcLine . HS.importLoc $ importDecl
               numTakes = max 0 (numDrops - 1)

           modifyImports importDecl (spec ^. sourceFile) (outputFile spec) numTakes numDrops

        AddImport importDecl -> do
           let numTakes = HS.srcLine . HS.importLoc $ importDecl
               numDrops = numTakes

           modifyImports importDecl (spec ^. sourceFile) (outputFile spec) numTakes numDrops

        NoImportChange
           | Just saveTo <- spec ^. saveToFile -> copyFile (spec ^. sourceFile) saveTo
           | otherwise                         -> return ()
   where
      modifyImports importDecl inputFile outputFile numTakes numDrops = do
         file <- TIO.readFile inputFile
         let importLine = HS.prettyPrint importDecl
             lines_     = lines . T.unpack $ file
             lines_'    = take numTakes lines_ ++ [importLine] ++ drop numDrops lines_
             file'      = T.pack . unlines $ lines_'

         TIO.writeFile outputFile file'

      outputFile spec
         | Just file <- spec ^. saveToFile = file
         | otherwise                       = spec ^. sourceFile
