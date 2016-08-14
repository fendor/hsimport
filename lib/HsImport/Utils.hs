
module HsImport.Utils
   ( firstSrcLine
   , lastSrcLine
   , srcSpan
   , declSrcLoc
   , importDecls
   ) where

import qualified Language.Haskell.Exts as HS

type SrcLine      = Int
type HsImportDecl = HS.ImportDecl HS.SrcSpanInfo
type HsModule     = HS.Module HS.SrcSpanInfo

firstSrcLine :: HsImportDecl -> SrcLine
firstSrcLine = HS.startLine . HS.importAnn


srcSpan :: HsImportDecl -> HS.SrcSpan
srcSpan = HS.srcInfoSpan . HS.importAnn


lastSrcLine :: HsImportDecl -> SrcLine
lastSrcLine = HS.srcSpanEndLine . srcSpan


declSrcLoc :: HS.Decl HS.SrcSpanInfo -> HS.SrcLoc
declSrcLoc decl = HS.SrcLoc srcFile srcLine srcCol
   where
      srcSpan = HS.srcInfoSpan . HS.ann $ decl
      srcFile = HS.srcSpanFilename srcSpan
      srcLine = HS.srcSpanStartLine srcSpan
      srcCol  = HS.srcSpanStartColumn srcSpan


importDecls :: HsModule -> [HsImportDecl]
importDecls (HS.Module _ _ _ imports _)            = imports
importDecls (HS.XmlPage _ _ _ _ _ _ _)             = []
importDecls (HS.XmlHybrid _ _ _ imports _ _ _ _ _) = imports
