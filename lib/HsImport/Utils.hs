
module HsImport.Utils
   ( declSrcLoc
   , importDecls
   ) where

import qualified SrcLoc as HS
import qualified HsSyn as HS
import HsImport.Types

declSrcLoc :: Decl -> SrcLoc
declSrcLoc decl = HS.srcSpanStart $ HS.getLoc $ decl

importDecls :: Module -> [ImportDecl]
importDecls = HS.hsmodImports
