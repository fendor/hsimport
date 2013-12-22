
module HsImport.Main
   ( hsImport
   ) where

import Control.Applicative
import Control.Lens
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.Haskell.Exts as HS
import HsImport.Imports
import HsImport.ImportSpec


hsImport :: ImportSpec -> IO ()
hsImport spec = do
   let imports = matchingImports (spec ^. moduleToImport) (spec ^. parsedSrcFile)
   if null imports
      then putStrLn "Nothing to do!"
      else do
         file <- TIO.readFile (spec ^. sourceFile)
         let srcLine    = HS.srcLine . HS.importLoc . last $ imports
             importLine = newImport (spec ^. moduleToImport) (spec ^. symbolToImport)
             lines_     = lines . T.unpack $ file
             lines_'    = take srcLine lines_ ++ [importLine] ++ drop srcLine lines_
             file'      = T.pack . unlines $ lines_'

         TIO.writeFile (spec ^. sourceFile) file'
   where
      newImport mod sym =
         "import " ++ mod ++ (if isJust sym then " (" ++ fromJust sym ++ ")" else "")
