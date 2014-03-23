
module HsImport.Symbol
   ( Symbol(..)
   ) where

type Name = String

-- | What of the symbol should be imported.
data Symbol = Symbol Name                -- ^ only the symbol should be imported
            | AllOfSymbol Name           -- ^ all constructors or methods of the symbol should be imported: Symbol(..)
            | SomeOfSymbol Name [String] -- ^ some constructors or methods of the symbol should be imported: Symbol(X, Y) 
            deriving (Show)
