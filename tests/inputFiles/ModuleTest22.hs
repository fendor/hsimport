module Blub where
import qualified Language.Haskell.Exts as HS

addSymbol :: HS.ImportDecl -> String -> HS.ImportDecl
addSymbol (id@HS.ImportDecl {HS.importSpecs = specs}) symbolName =
   id {HS.importSpecs = specs & _Just . _2 %~ (++ [HS.IVar $ hsName symbolName])}
