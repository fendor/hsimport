
module HsImport.Types where

import qualified HsSyn as HS
import qualified SrcLoc as HS
import qualified Lexer as HS
import qualified Name as HS
import qualified Module as HS

type SrcLine        = Int
type SrcColumn      = Int
type SrcSpan        = HS.SrcSpan
type SrcLoc         = HS.SrcLoc
type Decl           = HS.LHsDecl HS.GhcPs
type ImportDecl     = HS.LImportDecl HS.GhcPs
type ImportSpec     = Maybe (Bool, [ImportSpecList])
type ImportSpecList = HS.LIE HS.GhcPs
type Name           = HS.Name
type Module         = HS.HsModule HS.GhcPs
type ModuleName     = HS.ModuleName
type ErrorMessage   = String

data ParseResult = ParseResult
   { -- | the parse result
     result :: HS.ParseResult (HS.Located Module)

     -- | if the source file isn't completely parsable, because e.g.
     --   it contains incomplete Haskell code, then 'lastValidLine'
     --   contains the last line till the source is parsable
   , lastValidLine :: Maybe Int
   }

-- firstSrcLine :: Annotation -> SrcLine
-- firstSrcLine = minimum . map HS.srcSpanStartLine . srcSpans

-- lastSrcLine :: Annotation -> SrcLine
-- lastSrcLine = maximum . map HS.srcSpanEndLine . srcSpans

-- firstSrcColumn :: Annotation -> SrcColumn
-- firstSrcColumn = minimum . map HS.srcSpanStartColumn . srcSpans

-- lastSrcColumn :: Annotation -> SrcColumn
-- lastSrcColumn = maximum . map HS.srcSpanEndColumn . srcSpans

-- srcSpan :: Annotation -> SrcSpan
-- srcSpan ann@(HS.SrcSpanInfo srcSpan _, _) =
--    srcSpan { HS.srcSpanStartLine   = firstSrcLine ann
--            , HS.srcSpanStartColumn = firstSrcColumn ann
--            , HS.srcSpanEndLine     = lastSrcLine ann
--            , HS.srcSpanEndColumn   = lastSrcColumn ann
--            }

-- srcSpans :: Annotation -> [SrcSpan]
-- srcSpans (HS.SrcSpanInfo srcSpan _, comments) = srcSpan : commentSrcSpans
--    where
--       commentSrcSpans = map (\(HS.Comment _ srcSpan _) -> srcSpan) comments

-- noAnnotation :: Annotation
-- noAnnotation = (HS.noSrcSpan, [])
