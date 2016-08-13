
module HsImport.Utils
   ( importDeclSrcLine
   , spanSrcLine
   , declSrcLoc
   , importDecls
   ) where

import qualified Language.Haskell.Exts as HS

type SrcLine = Int

importDeclSrcLine :: HS.ImportDecl HS.SrcSpanInfo -> SrcLine
importDeclSrcLine = spanSrcLine . HS.importAnn


spanSrcLine :: HS.SrcSpanInfo -> SrcLine
spanSrcLine = HS.srcSpanStartLine . HS.srcInfoSpan


declSrcLoc :: HS.Decl HS.SrcSpanInfo -> HS.SrcLoc
declSrcLoc decl = HS.SrcLoc srcFile srcLine srcCol
   where
      srcSpan = HS.srcInfoSpan . HS.ann $ decl
      srcFile = HS.srcSpanFilename srcSpan
      srcLine = HS.srcSpanStartLine srcSpan
      srcCol  = HS.srcSpanStartColumn srcSpan


importDecls :: HS.Module HS.SrcSpanInfo -> [HS.ImportDecl HS.SrcSpanInfo]
importDecls (HS.Module _ _ _ imports _)            = imports
importDecls (HS.XmlPage _ _ _ _ _ _ _)             = []
importDecls (HS.XmlHybrid _ _ _ imports _ _ _ _ _) = imports
