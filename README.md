hsimport
========

[![Build Status](https://travis-ci.org/fendor/hsimport.svg?branch=master)](https://travis-ci.org/fendor/hsimport)

A command line program for extending the import list of a Haskell source file.

`hsimport` gets the module name and the symbol name to import as arguments,
parses the given source file using the library `haskell-src-exts` and then tries
to only extend the import list if it's necessary. If the symbol is already
imported or if the whole module is already imported, then the given source file
isn't changed.

Installation
------------

    cabal install hsimport

Examples
--------

    $> hsimport -m 'Control.Monad' SomeSource.hs
    => import Control.Monad

    $> hsimport -m 'Control.Monad' -s 'when' SomeSource.hs
    => import Control.Monad (when)

    $> hsimport -m 'Control.Monad' -q 'CM' SomeSource.hs
    => import qualified Control.Monad as CM

    $> hsimport -m 'Control.Monad' --as 'CM' SomeSource.hs
    => import Control.Monad as CM

    $> hsimport -m 'Data.Maybe' -s 'Maybe'
    => import Data.Maybe (Maybe)

    $> hsimport -m 'Data.Maybe' -s 'Maybe' -a
    => import Data.Maybe (Maybe(..))

    $> hsimport -m 'Data.Maybe' -s 'Maybe' -w 'Just'
    => import Data.Maybe (Maybe(Just))

    $> hsimport -m 'Data.Maybe' -s 'Maybe' -w 'Just' -w 'Nothing'
    => import Data.Maybe (Maybe(Just, Nothing))

Configuration
-------------

The easiest way to configure `hsimport` is by creating a `cabal` project.
An example for this is [here](<https://github.com/fendor/hsimport-config>).


The other way - which most likely isn't worth the hassle - is by writting a `~/.config/hsimport/hsimport.hs` file:

    -- ~/.config/hsimport/hsimport.hs
    import qualified Language.Haskell.Exts as HS
    import HsImport

    main :: IO ()
    main = hsimport $ defaultConfig { prettyPrint = prettyPrint, findImportPos = findImportPos }
       where
          -- This is a bogus implementation of prettyPrint, because it doesn't handle the
          -- qualified import case nor does it considers any explicitely imported or hidden symbols.
          prettyPrint :: ImportDecl -> String
          prettyPrint (ImportDecl { HS.importModule = HS.ModuleName _ modName }) =
             "import " ++ modName

          -- This findImportPos implementation will always add the new import declaration
          -- at the end of the current ones. The data type ImportPos has the two constructors
          -- After and Before.
          findImportPos :: ImportDecl -> [ImportDecl] -> Maybe ImportPos
          findImportPos         _             [] = Nothing
          findImportPos newImport currentImports = Just . After . last $ currentImports

The position of the configuration file depends on the result of `getUserConfigDir "hsimport"`,
which is a function from the package [xdg-basedir](<https://hackage.haskell.org/package/xdg-basedir>),
on linux like systems it is `~/.config/hsimport/hsimport.hs`.

If you've modified the configuration file, then the next call of `hsimport` will ensure a rebuild.
If you've installed `hsimport` with `cabal install`, without using a sandbox, then this should just work.

If you've build `hsimport` inside of a sandbox, then you most likely have to temporary modify the
`GHC_PACKAGE_PATH` for the next call of `hsimport`, to point `ghc` to the global database and
to the package database of the sandbox.

    # global package database
    $> export GLOBAL_PKG_DB=/usr/lib/ghc/package.conf.d/

    # hsimport sandbox package database
    $> export SANDBOX_PKG_DB=/home/you/hsimport-build-dir/.cabal-sandbox/*-packages.conf.d/

    $> GHC_PACKAGE_PATH=$GLOBAL_PKG_DB:$SANDBOX_PKG_DB hsimport --help

Text Editor Integration
-----------------------

[vim-hsimport](<https://github.com/dan-t/vim-hsimport>)

Command Line Usage
------------------

    $> hsimport --help
    hsimport [OPTIONS] [SOURCEFILE]
      A command line program for extending the import list of a Haskell source
      file.

    Common flags:
      -m --modulename=ITEM     The module to import
      -s --symbolname=ITEM     The symbol to import, if empty, the entire module
                               is imported
      -a --all                 All constructors or methods of the symbol should
                               be imported: 'Symbol(..)'
      -w --with=ITEM           The constructors or methods of the symbol
                               should be imported: 'Symbol(With)'
      -q --qualifiedname=ITEM  The name to use for a qualified module import
      -o --outputsrcfile=FILE  Save modified source file to file, if empty, the
                               source file is modified inplace
      -h --help                Display help message
      -v --version             Print version information

Issues
------

There is some rudimentarily handling for code using CPP, but the import statements
might be added at the wrong place, because the lines containing CPP directives
are ignored and therefore they aren't considered in the source line count.
