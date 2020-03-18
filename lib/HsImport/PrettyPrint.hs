{-# Language CPP #-}

module HsImport.PrettyPrint
   ( prettyPrint
   ) where

import DynFlags

import qualified Outputable as HS
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mconcat)
#endif

import HsImport.Types

prettyPrint :: DynFlags -> ImportDecl -> String
prettyPrint flags importDecl =
   -- remove newlines from pretty printed ImportDecl
   case lines $ HS.renderWithStyle flags (HS.ppr importDecl) (HS.defaultDumpStyle flags) of
        (fst : []  ) -> fst
        (fst : rest) -> mconcat $ fst : (map (' ' :) . map (dropWhile (== ' ')) $ rest)
        _            -> ""
