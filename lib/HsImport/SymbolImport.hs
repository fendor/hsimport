
module HsImport.SymbolImport
   ( SymbolImport(..)
   , Symbol(..)
   , symbol
   , isHiding
   , swapImport
   )
where

type Name = String

-- | What of the symbol should be taken.
data Symbol
   = Only Name            -- ^ only the symbol should be taken
   | AllOf Name           -- ^ all constructors or methods of the symbol should be taken: Symbol(..)
   | SomeOf Name [String] -- ^ some constructors or methods of the symbol should be taken: Symbol(X, Y)
   deriving (Show)


-- | The imported or from the import hidden symbol.
data SymbolImport
   = Import Symbol -- ^ the symbol to import
   | Hiding Symbol -- ^ the symbol to hide from the import
   deriving (Show)

-- | Retrieve the symbol out of the symbol import
symbol :: SymbolImport -> Symbol
symbol (Hiding s) = s
symbol (Import s) = s

-- | Check whether this symbol import is meant to hide a symbol.
isHiding :: SymbolImport -> Bool
isHiding (Hiding _) = True
isHiding (Import _) = False

-- | Swap the import style from hiding to import list and vice versa.
swapImport :: SymbolImport -> SymbolImport
swapImport (Hiding s) = Import s
swapImport (Import s) = Hiding s

