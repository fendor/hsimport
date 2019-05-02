
module HsImport.Utils
   ( firstSrcLine
   , lastSrcLine
   , srcSpan
   , declSrcLoc
   , importDecls
   ) where

import qualified Language.Haskell.Exts as HS
import HsImport.Types

declSrcLoc :: Decl -> SrcLoc
declSrcLoc decl = HS.SrcLoc srcFile srcLine srcCol
   where
      declSrcSpan = srcSpan . HS.ann $ decl
      srcFile     = HS.srcSpanFilename declSrcSpan
      srcLine     = HS.srcSpanStartLine declSrcSpan
      srcCol      = HS.srcSpanStartColumn declSrcSpan


importDecls :: Module -> [ImportDecl]
importDecls (HS.Module _ _ _ imports _)            = imports
importDecls (HS.XmlPage _ _ _ _ _ _ _)             = []
importDecls (HS.XmlHybrid _ _ _ imports _ _ _ _ _) = imports
