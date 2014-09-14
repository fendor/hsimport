
module HsImport.Utils
   ( srcLine
   , declSrcLoc
   , importDecls
   ) where

import qualified Language.Haskell.Exts as HS

type SrcLine = Int

srcLine :: HS.ImportDecl -> SrcLine
srcLine = HS.srcLine . HS.importLoc


declSrcLoc :: HS.Decl -> Maybe HS.SrcLoc
declSrcLoc decl =
   case decl of
        HS.TypeDecl srcLoc _ _ _            -> Just srcLoc
        HS.TypeFamDecl srcLoc _ _ _         -> Just srcLoc
        HS.ClosedTypeFamDecl srcLoc _ _ _ _ -> Just srcLoc
        HS.DataDecl srcLoc _ _ _ _ _ _      -> Just srcLoc
        HS.GDataDecl srcLoc _ _ _ _ _ _ _   -> Just srcLoc
        HS.DataFamDecl srcLoc _ _ _ _       -> Just srcLoc
        HS.TypeInsDecl srcLoc _ _           -> Just srcLoc
        HS.DataInsDecl srcLoc _ _ _ _       -> Just srcLoc
        HS.GDataInsDecl srcLoc _ _ _ _ _    -> Just srcLoc
        HS.ClassDecl srcLoc _ _ _ _ _       -> Just srcLoc
        HS.InstDecl srcLoc _ _ _ _ _ _      -> Just srcLoc
        HS.DerivDecl srcLoc _ _ _ _ _       -> Just srcLoc
        HS.InfixDecl srcLoc _ _ _           -> Just srcLoc
        HS.DefaultDecl srcLoc _             -> Just srcLoc
        HS.SpliceDecl srcLoc _              -> Just srcLoc
        HS.TypeSig srcLoc _ _               -> Just srcLoc
        HS.FunBind _                        -> Nothing
        HS.PatBind srcLoc _ _ _             -> Just srcLoc
        HS.ForImp srcLoc _ _ _ _ _          -> Just srcLoc
        HS.ForExp srcLoc _ _ _ _            -> Just srcLoc
        HS.RulePragmaDecl srcLoc _          -> Just srcLoc
        HS.DeprPragmaDecl srcLoc _          -> Just srcLoc
        HS.WarnPragmaDecl srcLoc _          -> Just srcLoc
        HS.InlineSig srcLoc _ _ _           -> Just srcLoc
        HS.InlineConlikeSig srcLoc _ _      -> Just srcLoc
        HS.SpecSig srcLoc _ _ _             -> Just srcLoc
        HS.SpecInlineSig srcLoc _ _ _ _     -> Just srcLoc
        HS.InstSig srcLoc _ _ _ _           -> Just srcLoc
        HS.AnnPragma srcLoc _               -> Just srcLoc
        HS.MinimalPragma srcLoc _           -> Just srcLoc


importDecls :: HS.Module -> [HS.ImportDecl]
importDecls (HS.Module _ _ _ _ _ imports _) = imports
