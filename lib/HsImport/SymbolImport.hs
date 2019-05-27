
module HsImport.SymbolImport
   ( SymbolImport(..)
   ) where

type Name = String

-- | What of the symbol should be imported.
data SymbolImport
   = Symbol Name                -- ^ only the symbol should be imported
   | AllOfSymbol Name           -- ^ all constructors or methods of the symbol should be imported: Symbol(..)
   | SomeOfSymbol Name [String] -- ^ some constructors or methods of the symbol should be imported: Symbol(X, Y)
   | HideSymbol Name        -- ^ Hide a symbol when importing
   deriving (Show)
