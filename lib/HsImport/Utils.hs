
module HsImport.Utils
   ( firstSrcLine
   , lastSrcLine
   , srcSpan
   , declSrcLoc
   , importDecls
   ) where

import qualified Language.Haskell.Exts as HS
import HsImport.Types

firstSrcLine :: (HS.Annotated ann) => ann Annotation -> SrcLine
firstSrcLine = HS.startLine . srcSpanInfo


lastSrcLine :: (HS.Annotated ann) => ann Annotation -> SrcLine
lastSrcLine = HS.srcSpanEndLine . srcSpan


srcSpan :: (HS.Annotated ann) => ann Annotation -> SrcSpan
srcSpan = HS.srcInfoSpan . srcSpanInfo


declSrcLoc :: Decl -> SrcLoc
declSrcLoc decl = HS.SrcLoc srcFile srcLine srcCol
   where
      declSrcSpan = srcSpan decl
      srcFile     = HS.srcSpanFilename declSrcSpan
      srcLine     = HS.srcSpanStartLine declSrcSpan
      srcCol      = HS.srcSpanStartColumn declSrcSpan


importDecls :: Module -> [ImportDecl]
importDecls (HS.Module _ _ _ imports _)            = imports
importDecls (HS.XmlPage _ _ _ _ _ _ _)             = []
importDecls (HS.XmlHybrid _ _ _ imports _ _ _ _ _) = imports
