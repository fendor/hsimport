
module HsImport.Types where

import qualified Language.Haskell.Exts as HS

type SrcLine     = Int
type SrcColumn   = Int
type SrcSpan     = HS.SrcSpan
type SrcLoc      = HS.SrcLoc
type Annotation  = (HS.SrcSpanInfo, [HS.Comment])
type Decl        = HS.Decl Annotation
type ImportDecl  = HS.ImportDecl Annotation
type ImportSpec  = HS.ImportSpec Annotation
type Name        = HS.Name Annotation
type Module      = HS.Module Annotation
type ModuleName  = String
type ParseResult = HS.ParseResult Module
type Error       = String

firstSrcLine :: Annotation -> SrcLine
firstSrcLine = minimum . map HS.srcSpanStartLine . srcSpans

lastSrcLine :: Annotation -> SrcLine
lastSrcLine = maximum . map HS.srcSpanEndLine . srcSpans

firstSrcColumn :: Annotation -> SrcColumn
firstSrcColumn = minimum . map HS.srcSpanStartColumn . srcSpans

lastSrcColumn :: Annotation -> SrcColumn
lastSrcColumn = maximum . map HS.srcSpanEndColumn . srcSpans

srcSpan :: Annotation -> SrcSpan
srcSpan ann@(HS.SrcSpanInfo srcSpan _, _) =
   srcSpan { HS.srcSpanStartLine   = firstSrcLine ann
           , HS.srcSpanStartColumn = firstSrcColumn ann
           , HS.srcSpanEndLine     = lastSrcLine ann
           , HS.srcSpanEndColumn   = lastSrcColumn ann
           }

srcSpans :: Annotation -> [SrcSpan]
srcSpans (HS.SrcSpanInfo srcSpan _, comments) = srcSpan : commentSrcSpans
   where
      commentSrcSpans = map (\(HS.Comment _ srcSpan _) -> srcSpan) comments

noAnnotation :: Annotation
noAnnotation = (HS.noSrcSpan, [])
