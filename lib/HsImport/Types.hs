
module HsImport.Types where

import qualified Language.Haskell.Exts as HS

type SrcLine     = Int
type SrcSpan     = HS.SrcSpan
type SrcLoc      = HS.SrcLoc
type Annotation  = HS.SrcSpanInfo
type Decl        = HS.Decl Annotation
type ImportDecl  = HS.ImportDecl Annotation
type ImportSpec  = HS.ImportSpec Annotation
type Name        = HS.Name Annotation
type Module      = HS.Module Annotation
type ModuleName  = String
type ParseResult = HS.ParseResult Module
type Error       = String

srcSpanInfo :: (HS.Annotated ann) => ann Annotation -> HS.SrcSpanInfo
srcSpanInfo = HS.ann
