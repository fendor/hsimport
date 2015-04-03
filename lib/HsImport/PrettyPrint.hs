{-# Language CPP #-}

module HsImport.PrettyPrint
   ( prettyPrint
   ) where

import qualified Language.Haskell.Exts as HS

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mconcat)
#endif


prettyPrint :: HS.ImportDecl -> String
prettyPrint importDecl =
   -- remove newlines from pretty printed ImportDecl
   case lines $ HS.prettyPrint importDecl of
        (fst : []  ) -> fst
        (fst : rest) -> mconcat $ fst : (map (' ' :) . map (dropWhile (== ' ')) $ rest)
        _            -> ""
