
module HsImport.PrettyPrint
   ( prettyPrint
   ) where

import Data.Monoid (mconcat)
import qualified Language.Haskell.Exts as HS


prettyPrint :: HS.ImportDecl -> String
prettyPrint importDecl =
   -- remove newlines from pretty printed ImportDecl
   case lines $ HS.prettyPrint importDecl of
        (fst : []  ) -> fst
        (fst : rest) -> mconcat $ fst : (map (' ' :) . map (dropWhile (== ' ')) $ rest)
        _            -> ""
