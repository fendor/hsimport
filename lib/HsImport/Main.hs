{-# Language PatternGuards, CPP #-}

module HsImport.Main
   ( hsimport
   , hsimportWithArgs
   ) where

import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (isJust)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Config.Dyre as Dyre
import qualified Language.Haskell.Exts as HS
import HsImport.ImportChange
import HsImport.ImportSpec
import HsImport.ImportPos (ImportPos(..))
import qualified HsImport.Args as Args
import HsImport.Config
import HsImport.Utils

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif


hsimport :: Config -> IO ()
hsimport = Dyre.wrapMain $ Dyre.defaultParams
   { Dyre.projectName = "hsimport"
   , Dyre.realMain    = realMain
   , Dyre.showError   = \config err -> config { configError = Just err }
   }
   where
      realMain :: Config -> IO ()
      realMain config = do
         case configError config of
              Just error -> hPutStrLn stderr ("hsimport: " ++ error) >> exitFailure
              _          -> return ()

         args     <- Args.hsImportArgs
         maybeErr <- hsimportWithArgs config args
         case maybeErr of
              Just err -> hPutStrLn stderr ("hsimport: " ++ err) >> exitFailure
              _        -> exitSuccess


type Error = String
hsimportWithArgs :: Config -> Args.HsImportArgs -> IO (Maybe Error)
hsimportWithArgs config args = do
   maybeSpec <- hsImportSpec args
   case maybeSpec of
        Left  error -> return $ Just error
        Right spec  -> hsimportWithSpec config spec >> return Nothing


hsimportWithSpec :: Config -> ImportSpec -> IO ()
hsimportWithSpec Config { prettyPrint = prettyPrint, findImportPos = findImportPos } spec = do
   let impChanges = importChanges (moduleImport spec) (symbolImport spec) (parsedSrcFile spec)

   srcLines <- lines . T.unpack <$> TIO.readFile (sourceFile spec)
   let srcLines' = applyChanges srcLines impChanges
   when (srcLines' /= srcLines || isJust (saveToFile spec)) $
      TIO.writeFile (outputFile spec) (T.pack $ unlines srcLines')

   where
      applyChanges = foldl' applyChange

      applyChange srcLines (ReplaceImportAt srcSpan importDecl) =
         let numTakes = max 0 (HS.srcSpanStartLine srcSpan - 1)
             numDrops = HS.srcSpanEndLine srcSpan
             in take numTakes srcLines ++ [prettyPrint importDecl] ++ drop numDrops srcLines

      applyChange srcLines (AddImportAfter srcLine importDecl) =
         let numTakes = srcLine
             numDrops = numTakes
             in take numTakes srcLines ++ [prettyPrint importDecl] ++ drop numDrops srcLines

      applyChange srcLines (AddImportAtEnd importDecl) =
         srcLines ++ [prettyPrint importDecl]

      applyChange srcLines (FindImportPos importDecl) =
         case findImportPos importDecl allImportDecls of
              Just (After impDecl)  -> applyChange srcLines (AddImportAfter (lastSrcLine impDecl)
                                                                            importDecl)
              Just (Before impDecl) -> applyChange srcLines (AddImportAfter (max 0 (firstSrcLine impDecl - 1))
                                                                            importDecl)
              _                     -> applyChange srcLines (AddImportAfter (lastSrcLine . last $ allImportDecls)
                                                                            importDecl)

      applyChange srcLines NoImportChange = srcLines

      outputFile spec
         | Just file <- saveToFile spec = file
         | otherwise                    = sourceFile spec

      allImportDecls = importDecls $ parsedSrcFile spec
