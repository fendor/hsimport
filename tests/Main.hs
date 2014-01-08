
module Main where

import Control.Monad (guard)
import System.Console.CmdArgs (cmdArgsApply, cmdArgsMode)
import System.Console.CmdArgs.Explicit (processValue)
import HsImport (hsImport, hsImportSpec)
import HsImport.Args (hsImportArgsOptions)
import Test.Tasty
import Test.Tasty.Golden
import System.FilePath

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [moduleTests, symbolTests]

moduleTests :: TestTree
moduleTests = testGroup "Module Tests"
   [ hsImportTest "ModuleTest0" "Foo.Bar" "" ""
   , hsImportTest "ModuleTest1" "Foo.Bar" "" ""
   , hsImportTest "ModuleTest2" "Foo.Bar.Blub" "" ""
   , hsImportTest "ModuleTest3" "Control.Monad" "" ""
   , hsImportTest "ModuleTest4" "Ugah.Argh2" "" ""
   , hsImportTest "ModuleTest5" "Control.Monad" "" ""
   , hsImportTest "ModuleTest6" "Control.Monad" "" ""
   , hsImportTest "ModuleTest7" "Control.Monad" "" ""
   , hsImportTest "ModuleTest8" "Control.Monad" "" ""
   , hsImportTest "ModuleTest9" "Control.Monad" "" ""
   , hsImportTest "ModuleTest10" "Control.Monad" "" ""
   , hsImportTest "ModuleTest11" "Control.Monad" "" ""
   , hsImportTest "ModuleTest12" "Control.Monad" "" ""
   , hsImportTest "ModuleTest13" "Control.Monad" "" ""
   , hsImportTest "ModuleTest14" "Control.Monad" "" ""
   , hsImportTest "ModuleTest15" "Control.Monad" "" ""
   , hsImportTest "ModuleTest16" "Control.Monad" "" ""
   , hsImportTest "ModuleTest17" "Control.Monad" "" ""
   , hsImportTest "ModuleTest18" "Control.Monad" "" ""
   , hsImportTest "ModuleTest19" "Control.Monad" "" ""
   , hsImportTest "ModuleTest20" "Control.Monad" "" ""
   , hsImportTest "ModuleTest21" "Control.Monad" "" ""
   , hsImportTest "ModuleTest22" "Control.Monad" "" ""
   , hsImportTest "ModuleTest23" "Control.Monad" "" "CM"
   , hsImportTest "ModuleTest24" "Control.Monad" "" "CM"
   , hsImportTest "ModuleTest25" "Control.Monad" "" "Control.Monad"
   , hsImportTest "ModuleTest26" "Control.Monad" "" ""
   ]


symbolTests :: TestTree
symbolTests = testGroup "Symbol Tests"
   [ hsImportTest "SymbolTest0" "Foo.Bar" "foo" ""
   , hsImportTest "SymbolTest1" "Foo.Bar" "foo" ""
   , hsImportTest "SymbolTest2" "Foo.Bar.Blub" "foo" ""
   , hsImportTest "SymbolTest3" "Control.Monad" "when" ""
   , hsImportTest "SymbolTest4" "Ugah.Argh2" "argh" ""
   , hsImportTest "SymbolTest5" "Control.Monad" "when" ""
   , hsImportTest "SymbolTest6" "Control.Monad" "unless" ""
   , hsImportTest "SymbolTest7" "Control.Monad" "unless" ""
   , hsImportTest "SymbolTest8" "Control.Monad" "unless" ""
   , hsImportTest "SymbolTest9" "Control.Monad" "unless" ""
   , hsImportTest "SymbolTest10" "Control.Applicative" "<$>" ""
   , hsImportTest "SymbolTest11" "Control.Applicative" "<$>" ""
   , hsImportTest "SymbolTest12" "Control.Applicative" "<$>" ""
   , hsImportTest "SymbolTest13" "Control.Applicative" "<*" ""
   , hsImportTest "SymbolTest14" "Control.Applicative" "<*" ""
   , hsImportTest "SymbolTest15" "Control.Applicative" "*>" ""
   , hsImportTest "SymbolTest16" "Control.Monad" "when" "CM"
   , hsImportTest "SymbolTest17" "Control.Monad" "when" "CM"
   , hsImportTest "SymbolTest18" "Data.Text" "Text" ""
   ]


hsImportTest :: String -> String -> String -> String -> TestTree
hsImportTest testName moduleName symbolName qualifiedName =
   goldenVsFileDiff testName diff goldenFile outputFile command
   where
      command = do
        args <- cmdArgsApply (processValue (cmdArgsMode hsImportArgsOptions) params)
        Right hsImportSpec <- hsImportSpec args
        hsImport hsImportSpec

      diff ref new = ["diff", "-u", ref, new]

      params      = concat [moduleParam, symbolParam, qualParam, outputParam, return inputFile]
      moduleParam = [ "-m",  moduleName ]
      symbolParam = guard (not $ null symbolName) >> [ "-s", symbolName ]
      qualParam   = guard (not $ null qualifiedName) >> [ "-q", qualifiedName ]
      outputParam = [ "-o", outputFile ]

      goldenFile = "tests" </> "goldenFiles" </> testName <.> "hs"
      outputFile = "tests" </> "outputFiles" </> testName <.> "hs"
      inputFile  = "tests" </> "inputFiles"  </> testName <.> "hs"
