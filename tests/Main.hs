
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import qualified Language.Haskell.Exts as HS
import HsImport.Main (hsimport_)
import HsImport.ImportSpec (hsImportSpec)
import qualified HsImport.Config as C
import qualified HsImport.Args as A
import qualified HsImport.ImportPos as P

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [moduleTests, symbolTests]

moduleTests :: TestTree
moduleTests = testGroup "Module Tests"
   [ test "ModuleTest0" $ A.defaultArgs { A.moduleName = "Foo.Bar" }
   , test "ModuleTest1" $ A.defaultArgs { A.moduleName = "Foo.Bar" }
   , test "ModuleTest2" $ A.defaultArgs { A.moduleName = "Foo.Bar.Blub" }
   , test "ModuleTest3" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest4" $ A.defaultArgs { A.moduleName = "Ugah.Argh2" }
   , test "ModuleTest5" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest6" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest7" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest8" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest9" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest10" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest11" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest12" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest13" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest14" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest15" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest16" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest17" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest18" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest19" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest20" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest21" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest22" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test "ModuleTest23" $ A.defaultArgs { A.moduleName = "Control.Monad", A.qualifiedName = "CM" }
   , test "ModuleTest24" $ A.defaultArgs { A.moduleName = "Control.Monad", A.qualifiedName = "CM" }
   , test "ModuleTest25" $ A.defaultArgs { A.moduleName = "Control.Monad", A.qualifiedName = "Control.Monad" }
   , test "ModuleTest26" $ A.defaultArgs { A.moduleName = "Control.Monad" }
   , test_ "ModuleTest27" (C.defaultConfig { C.findImportPos = importPosBeforeFirst }) (A.defaultArgs { A.moduleName = "Control.Monad" })
   , test_ "ModuleTest28" (C.defaultConfig { C.findImportPos = importPosAfterFirst }) (A.defaultArgs { A.moduleName = "Control.Monad" })
   , test_ "ModuleTest29" (C.defaultConfig { C.findImportPos = importPosBeforeLast }) (A.defaultArgs { A.moduleName = "Control.Monad" })
   , test_ "ModuleTest30" (C.defaultConfig { C.findImportPos = importPosAfterLast }) (A.defaultArgs { A.moduleName = "Control.Monad" })
   ]


symbolTests :: TestTree
symbolTests = testGroup "Symbol Tests"
   [ test "SymbolTest0" $ A.defaultArgs { A.moduleName = "Foo.Bar", A.symbolName = "foo" }
   , test "SymbolTest1" $ A.defaultArgs { A.moduleName = "Foo.Bar", A.symbolName = "foo" }
   , test "SymbolTest2" $ A.defaultArgs { A.moduleName = "Foo.Bar.Blub", A.symbolName = "foo" }
   , test "SymbolTest3" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "when" }
   , test "SymbolTest4" $ A.defaultArgs { A.moduleName = "Ugah.Argh2", A.symbolName = "argh" }
   , test "SymbolTest5" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "when" }
   , test "SymbolTest6" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "unless" }
   , test "SymbolTest7" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "unless" }
   , test "SymbolTest8" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "unless" }
   , test "SymbolTest9" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "unless" }
   , test "SymbolTest10" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "<$>" }
   , test "SymbolTest11" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "<$>" }
   , test "SymbolTest12" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "<$>" }
   , test "SymbolTest13" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "<*" }
   , test "SymbolTest14" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "<*" }
   , test "SymbolTest15" $ A.defaultArgs { A.moduleName = "Control.Applicative", A.symbolName = "*>" }
   , test "SymbolTest16" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "when", A.qualifiedName = "CM" }
   , test "SymbolTest17" $ A.defaultArgs { A.moduleName = "Control.Monad", A.symbolName = "when", A.qualifiedName = "CM" }
   , test "SymbolTest18" $ A.defaultArgs { A.moduleName = "Data.Text", A.symbolName = "Text" }
   , test "SymbolTest19" $ A.defaultArgs { A.moduleName = "Data.List", A.symbolName = "foldl'" }
   , test "SymbolTest20" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.all = True }
   , test "SymbolTest21" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.all = True }
   , test "SymbolTest22" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.with = ["Just"] }
   , test "SymbolTest23" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.all = True, A.with = ["Just"] }
   , test "SymbolTest24" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.with = ["Just"] }
   , test "SymbolTest25" $ A.defaultArgs { A.moduleName = "Data.Maybe", A.symbolName = "Maybe", A.with = ["Nothing", "Just"] }
   , test "SymbolTest26" $ A.defaultArgs { A.moduleName = "Foo", A.symbolName = "bar" }
   , test "SymbolTest27" $ A.defaultArgs { A.moduleName = "Ugah.Blub", A.symbolName = "g" }
   , test "SymbolTest28" $ A.defaultArgs { A.moduleName = "Ugah.Blub", A.symbolName = "d" }
   , test_ "SymbolTest29" (C.defaultConfig { C.prettyPrint = prettyPrint }) (A.defaultArgs { A.moduleName = "X.Y", A.symbolName = "x" })
   , test_ "SymbolTest30" (C.defaultConfig { C.prettyPrint = prettyPrint }) (A.defaultArgs { A.moduleName = "X.Y", A.symbolName = "x" })
   , test "SymbolTest31" $ A.defaultArgs { A.moduleName = "Ugah.Blub", A.symbolName = "d" }
   , test "SymbolTest32" $ A.defaultArgs { A.moduleName = "Control.Foo", A.symbolName = "foo" }
   ]


test :: String -> A.HsImportArgs -> TestTree
test testName args = test_ testName C.defaultConfig args


test_ :: String -> C.Config -> A.HsImportArgs -> TestTree
test_ testName config args =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         spec <- hsImportSpec (args { A.inputSrcFile = inputFile, A.outputSrcFile = outputFile })
         case spec of
              Left error  -> hPutStrLn stderr ("hsimport: " ++ error)
              Right spec_ -> hsimport_ config spec_

      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"


prettyPrint :: HS.ImportDecl -> String
prettyPrint HS.ImportDecl { HS.importModule = HS.ModuleName modName, HS.importSpecs = Just (False, syms) } =
   "import " ++ modName ++ " ( " ++ ppSyms ++ " )"
   where
      ppSyms = intercalate " , " symNames
      symNames = map symName syms

      symName (HS.IVar (HS.Ident name)) = name
      symName _ = ""

prettyPrint _ = "Uupps"


importPosAfterLast :: HS.ImportDecl -> [HS.ImportDecl] -> Maybe P.ImportPos
importPosAfterLast _ []      = Nothing
importPosAfterLast _ imports = Just . P.After . last $ imports


importPosBeforeLast :: HS.ImportDecl -> [HS.ImportDecl] -> Maybe P.ImportPos
importPosBeforeLast _ []      = Nothing
importPosBeforeLast _ imports = Just . P.Before . last $ imports


importPosAfterFirst :: HS.ImportDecl -> [HS.ImportDecl] -> Maybe P.ImportPos
importPosAfterFirst _ []      = Nothing
importPosAfterFirst _ imports = Just . P.After . head $ imports


importPosBeforeFirst :: HS.ImportDecl -> [HS.ImportDecl] -> Maybe P.ImportPos
importPosBeforeFirst _ []      = Nothing
importPosBeforeFirst _ imports = Just . P.Before . head $ imports
