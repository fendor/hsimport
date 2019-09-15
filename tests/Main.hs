
module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.UTF8 as BS
import qualified Language.Haskell.Exts as HS
import qualified HsImport as HI
import qualified HsImport.Parse as HIP
import HsImport.Types

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [moduleTests, symbolTests, replaceCppTests, parseTests]

moduleTests :: TestTree
moduleTests = testGroup "Module Tests"
   [ test "ModuleTest0" $ HI.defaultArgs { HI.moduleName = "Foo.Bar" }
   , test "ModuleTest1" $ HI.defaultArgs { HI.moduleName = "Foo.Bar" }
   , test "ModuleTest2" $ HI.defaultArgs { HI.moduleName = "Foo.Bar.Blub" }
   , test "ModuleTest3" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest4" $ HI.defaultArgs { HI.moduleName = "Ugah.Argh2" }
   , test "ModuleTest5" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest6" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest7" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest8" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest9" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest10" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest11" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest12" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest13" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest14" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest15" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest16" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest17" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest18" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest19" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest20" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest21" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest22" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest23" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.qualifiedName = "CM" }
   , test "ModuleTest24" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.qualifiedName = "CM" }
   , test "ModuleTest25" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.qualifiedName = "Control.Monad" }
   , test "ModuleTest26" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , configTest "ModuleTest27" (HI.defaultConfig { HI.findImportPos = importPosBeforeFirst }) (HI.defaultArgs { HI.moduleName = "Control.Monad" })
   , configTest "ModuleTest28" (HI.defaultConfig { HI.findImportPos = importPosAfterFirst }) (HI.defaultArgs { HI.moduleName = "Control.Monad" })
   , configTest "ModuleTest29" (HI.defaultConfig { HI.findImportPos = importPosBeforeLast }) (HI.defaultArgs { HI.moduleName = "Control.Monad" })
   , configTest "ModuleTest30" (HI.defaultConfig { HI.findImportPos = importPosAfterLast }) (HI.defaultArgs { HI.moduleName = "Control.Monad" })
   , test "ModuleTest31" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.as = "CM" }
   , test "ModuleTest32" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest33" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest34" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest35" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest36" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest37" $ HI.defaultArgs { HI.moduleName = "Control.Monad" }
   , test "ModuleTest38" $ HI.defaultArgs { HI.moduleName = "Data.Text" }
   ]


symbolTests :: TestTree
symbolTests = testGroup "Symbol Tests"
   [ test "SymbolTest0" $ HI.defaultArgs { HI.moduleName = "Foo.Bar", HI.symbolName = "foo" }
   , test "SymbolTest1" $ HI.defaultArgs { HI.moduleName = "Foo.Bar", HI.symbolName = "foo" }
   , test "SymbolTest2" $ HI.defaultArgs { HI.moduleName = "Foo.Bar.Blub", HI.symbolName = "foo" }
   , test "SymbolTest3" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "when" }
   , test "SymbolTest4" $ HI.defaultArgs { HI.moduleName = "Ugah.Argh2", HI.symbolName = "argh" }
   , test "SymbolTest5" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "when" }
   , test "SymbolTest6" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "unless" }
   , test "SymbolTest7" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "unless" }
   , test "SymbolTest8" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "unless" }
   , test "SymbolTest9" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "unless" }
   , test "SymbolTest10" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "<$>" }
   , test "SymbolTest11" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "<$>" }
   , test "SymbolTest12" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "<$>" }
   , test "SymbolTest13" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "<*" }
   , test "SymbolTest14" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "<*" }
   , test "SymbolTest15" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "*>" }
   , test "SymbolTest16" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "when", HI.qualifiedName = "CM" }
   , test "SymbolTest17" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "when", HI.qualifiedName = "CM" }
   , test "SymbolTest18" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text" }
   , test "SymbolTest19" $ HI.defaultArgs { HI.moduleName = "Data.List", HI.symbolName = "foldl'" }
   , test "SymbolTest20" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.all = True }
   , test "SymbolTest21" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.all = True }
   , test "SymbolTest22" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.with = ["Just"] }
   , test "SymbolTest23" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.all = True, HI.with = ["Just"] }
   , test "SymbolTest24" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.with = ["Just"] }
   , test "SymbolTest25" $ HI.defaultArgs { HI.moduleName = "Data.Maybe", HI.symbolName = "Maybe", HI.with = ["Nothing", "Just"] }
   , test "SymbolTest26" $ HI.defaultArgs { HI.moduleName = "Foo", HI.symbolName = "bar" }
   , test "SymbolTest27" $ HI.defaultArgs { HI.moduleName = "Ugah.Blub", HI.symbolName = "g" }
   , test "SymbolTest28" $ HI.defaultArgs { HI.moduleName = "Ugah.Blub", HI.symbolName = "d" }
   , configTest "SymbolTest29" (HI.defaultConfig { HI.prettyPrint = prettyPrint }) (HI.defaultArgs { HI.moduleName = "X.Y", HI.symbolName = "x" })
   , configTest "SymbolTest30" (HI.defaultConfig { HI.prettyPrint = prettyPrint }) (HI.defaultArgs { HI.moduleName = "X.Y", HI.symbolName = "x" })
   , test "SymbolTest31" $ HI.defaultArgs { HI.moduleName = "Ugah.Blub", HI.symbolName = "d" }
   , test "SymbolTest32" $ HI.defaultArgs { HI.moduleName = "Control.Foo", HI.symbolName = "foo" }
   , test "SymbolTest33" $ HI.defaultArgs { HI.moduleName = "Control.Monad", HI.symbolName = "mapM_", HI.hiding = True }
   , test "SymbolTest34" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "isPrefixOf", HI.hiding = True }
   , test "SymbolTest35" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "isInfixOf", HI.hiding = True }
   , test "SymbolTest36" $ HI.defaultArgs { HI.qualifiedName = "T", HI.moduleName = "Data.Text", HI.symbolName = "isInfixOf", HI.hiding = True }
   , test "SymbolTest37" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.all = True, HI.hiding = True }
   , test "SymbolTest38" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["A", "B", "C"], HI.hiding = True }
   , test "SymbolTest39" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.all = True, HI.as = "T", HI.hiding = True }
   , test "SymbolTest40" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["A", "B", "C"], HI.as = "T", HI.hiding = True }
   , test "SymbolTest41" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "t", HI.hiding = True }
   , test "SymbolTest42" $ HI.defaultArgs { HI.moduleName = "Control.Applicative", HI.symbolName = "z", HI.hiding = True }
   , test "SymbolTest43" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "isInfixOf" }
   , test "SymbolTest44" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.all = True, HI.hiding = True }
   , test "SymbolTest45" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["A"], HI.hiding = True }
   , test "SymbolTest46" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["A"], HI.hiding = True }
   , test "SymbolTest47" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.all = True, HI.hiding = True }
   , test "SymbolTest48" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "isInfixOf" }
   , test "SymbolTest49" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.hiding = True }
   , test "SymbolTest50" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["B", "C"], HI.hiding = True }
   , failedTest "SymbolTest51" $ HI.defaultArgs { HI.moduleName = "Data.Text", HI.symbolName = "Text", HI.with = ["A"], HI.hiding = True }
   ]


replaceCppTests :: TestTree
replaceCppTests = testGroup "ReplaceCpp Tests"
   [ replaceCppTest "ReplaceCppTest1"
   , replaceCppTest "ReplaceCppTest2"
   , replaceCppTest "ReplaceCppTest3"
   ]


parseTests :: TestTree
parseTests = testGroup "Parse Tests"
   [ parseTest "ParseTest1"
   , parseTest "ParseTest2"
   ]


test :: String -> HI.HsImportArgs -> TestTree
test testName args = configTest testName HI.defaultConfig args


failedTest :: String -> HI.HsImportArgs -> TestTree
failedTest testName args =
   goldenVsStringDiff testName diff goldenFile command
   where
      command = do
         Just message <- HI.hsimportWithArgs HI.defaultConfig (args { HI.inputSrcFile = inputFile, HI.outputSrcFile = outputFile })
         return . BS.fromString $ message

      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"


configTest :: String -> HI.Config -> HI.HsImportArgs -> TestTree
configTest testName config args =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         maybeErr <- HI.hsimportWithArgs config (args { HI.inputSrcFile = inputFile, HI.outputSrcFile = outputFile })
         case maybeErr of
              Just err -> hPutStrLn stderr ("hsimport: " ++ err)
              _        -> return ()

      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"


replaceCppTest :: String -> TestTree
replaceCppTest testName =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         contents <- readFile inputFile
         writeFile outputFile $ HIP.replaceCpp contents

      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"


parseTest :: String -> TestTree
parseTest testName =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
         Right (ParseResult (HS.ParseOk _) lastValidLine) <- HIP.parseFile inputFile
         contents <- readFile inputFile
         case lastValidLine of
              Just line -> writeFile outputFile $ unlines . take line . lines $ contents
              Nothing -> writeFile outputFile contents

      diff ref new = ["diff", "-u", ref, new]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"


prettyPrint :: HI.ImportDecl -> String
prettyPrint HS.ImportDecl { HS.importModule = HS.ModuleName _ modName, HS.importSpecs = Just (HS.ImportSpecList _ False syms) } =
   "import " ++ modName ++ " ( " ++ ppSyms ++ " )"
   where
      ppSyms = intercalate " , " symNames
      symNames = map symName syms

      symName (HS.IVar _ (HS.Ident _ name)) = name
      symName _ = ""

prettyPrint _ = "Uupps"


importPosAfterLast :: HI.ImportDecl -> [HI.ImportDecl] -> Maybe HI.ImportPos
importPosAfterLast _ []      = Nothing
importPosAfterLast _ imports = Just . HI.After . last $ imports


importPosBeforeLast :: HI.ImportDecl -> [HI.ImportDecl] -> Maybe HI.ImportPos
importPosBeforeLast _ []      = Nothing
importPosBeforeLast _ imports = Just . HI.Before . last $ imports


importPosAfterFirst :: HI.ImportDecl -> [HI.ImportDecl] -> Maybe HI.ImportPos
importPosAfterFirst _ []      = Nothing
importPosAfterFirst _ imports = Just . HI.After . head $ imports


importPosBeforeFirst :: HI.ImportDecl -> [HI.ImportDecl] -> Maybe HI.ImportPos
importPosBeforeFirst _ []      = Nothing
importPosBeforeFirst _ imports = Just . HI.Before . head $ imports
