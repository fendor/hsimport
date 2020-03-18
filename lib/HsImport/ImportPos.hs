{-# Language CPP #-}

module HsImport.ImportPos
   ( findImportPos
   , ImportPos(..)
   , matchingImports
   , bestMatchingImport
   ) where

import qualified HsSyn as HS
import qualified Module as HS
import qualified SrcLoc as HS
import Data.List.Index (ifoldl')
import Data.List.Split (splitOn)
import HsImport.Types

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

-- | Where a new import declaration should be added.
data ImportPos = Before ImportDecl -- ^ before the specified import declaration
               | After  ImportDecl -- ^ after the specified import declaration
               


-- | Returns the position where the import declaration for the
--   new import should be put into the list of import declarations.
findImportPos :: ImportDecl -> [ImportDecl] -> Maybe ImportPos
findImportPos (HS.L _ newImport) imports = After <$> bestMatchingImport moduleName imports
   where
      HS.L _ moduleName = HS.ideclName newImport


-- | Returns all import declarations having the same module name.
matchingImports :: ModuleName -> [ImportDecl] -> [ImportDecl]
matchingImports moduleName imports =
   [ i
   | i@(HS.L _ HS.ImportDecl {HS.ideclName = HS.L _ name}) <- imports
   , moduleName == name
   ]


-- | Returns the best matching import declaration for the given module name.
--   E.g. if the module name is "Foo.Bar.Boo", then "Foo.Bar" is considered
--   better matching than "Foo".
bestMatchingImport :: ModuleName -> [ImportDecl] -> Maybe ImportDecl
bestMatchingImport _          []      = Nothing
bestMatchingImport moduleName imports =
   case ifoldl' computeMatches Nothing splittedMods of
        Just (idx, _) -> Just $ imports !! idx
        _             -> Nothing
   where
      computeMatches :: Maybe (Int, Int) -> Int -> [String] -> Maybe (Int, Int)
      computeMatches matches idx mod =
         let num' = numMatches splittedMod mod
             in case matches of
                     Just (_, num) | num' >= num -> Just (idx, num')
                                   | otherwise   -> matches

                     Nothing | num' > 0  -> Just (idx, num')
                             | otherwise -> Nothing
         where
            numMatches = loop 0
               where
                  loop num (a:as) (b:bs)
                     | a == b    = loop (num + 1) as bs
                     | otherwise = num

                  loop num [] _ = num
                  loop num _ [] = num

      splittedMod  = splitOn "." $ HS.moduleNameString moduleName
      splittedMods = [ splitOn "." $ HS.moduleNameString name
                     | (HS.L _ HS.ImportDecl {HS.ideclName = HS.L _ name}) <- imports
                     ]
