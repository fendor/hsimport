
module HsImport.SymbolImport
   ( SymbolImport(..)
   , Symbol(..)
   , symbol
   , isHiding
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

symbol :: SymbolImport -> Symbol
symbol (Hiding s) = s
symbol (Import s) = s

isHiding :: SymbolImport -> Bool
isHiding (Hiding _) = True
isHiding (Import _) = False
