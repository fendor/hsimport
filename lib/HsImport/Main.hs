{-# Language PatternGuards, CPP #-}

module HsImport.Main
   ( hsimport
   , hsimportWithArgs
   ) where

import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (isJust, mapMaybe)
import Data.List (foldl', partition)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Config.Dyre as Dyre
import qualified Language.Haskell.Exts as HS
import HsImport.ImportChange
import HsImport.HsImportSpec
import HsImport.ImportPos (ImportPos(..))
import qualified HsImport.Args as Args
import HsImport.Config
import HsImport.Utils
import HsImport.Types

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


hsimportWithArgs :: Config -> Args.HsImportArgs -> IO (Maybe ErrorMessage)
hsimportWithArgs config args = do
   maybeSpec <- hsImportSpec args
   case maybeSpec of
        Left  error -> return $ Just error
        Right spec  -> hsimportWithSpec config spec


hsimportWithSpec :: Config -> HsImportSpec -> IO (Maybe ErrorMessage)
hsimportWithSpec Config { prettyPrint = prettyPrint, findImportPos = findImportPos } spec = do
   let impChanges = importChanges (moduleImport spec) (symbolImport spec) (parsedSrcFile spec)
   case partition hasImportError impChanges of

      ([], changes) -> do
         srcLines <- lines . T.unpack <$> TIO.readFile (sourceFile spec)
         let srcLines' = applyChanges srcLines changes
         when (srcLines' /= srcLines || isJust (saveToFile spec)) $
            TIO.writeFile (outputFile spec) (T.pack $ unlines srcLines')
         return Nothing

      (errors, _) ->
         return (Just (unlines $ mapMaybe toErrorMessage errors))

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
              Just (After impDecl)  -> applyChange srcLines (AddImportAfter (lastSrcLine . HS.ann $ impDecl)
                                                                            importDecl)
              Just (Before impDecl) -> applyChange srcLines (AddImportAfter (max 0 ((firstSrcLine . HS.ann $ impDecl) - 1))
                                                                            importDecl)
              _                     -> applyChange srcLines (AddImportAfter (lastSrcLine . HS.ann . last $ allImportDecls)
                                                                            importDecl)

      applyChange srcLines NoImportChange = srcLines

      applyChange _ (ImportError _) = error "hsimportWithSpec.ImportError: encountered an ImportError although there should "

      outputFile spec
         | Just file <- saveToFile spec = file
         | otherwise                    = sourceFile spec

      allImportDecls = importDecls $ parsedSrcFile spec

hasImportError :: ImportChange -> Bool
hasImportError (ImportError _) = True
hasImportError _ = False

toErrorMessage :: ImportChange -> Maybe ErrorMessage
toErrorMessage (ImportError err) = Just err
toErrorMessage _ = Nothing
