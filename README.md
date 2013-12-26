hsimport
========

A command line program for extending the import list of a Haskell source file.

`hsimport` gets the module name and the symbol name to import as arguments,
parses the given source file by using the library `haskell-src-exts` and than
tries to only extend the import list if it's necessary. If the symbol is already
imported or if the whole module is already imported, than the given source file
isn't changed.

Installation
------------

    cabal install hsimport

Text Editor Integration
-----------------------

[vim-hsimport](<https://github.com/dan-t/vim-hsimport>)

Command Line Usage
------------------

    dan@machine ~> hsimport --help
    hsimport [OPTIONS] [SOURCEFILE]
      A command line program for extending the import list of a Haskell source
      file.
    
    Common flags:
      -m --modulename=ITEM     The module to import
      -s --symbolname=ITEM     The symbol to import, if empty, the entire module
                               is imported
      -o --outputsrcfile=FILE  Save modified source file to file, if empty, the
                               source file is modified inplace
      -h --help                Display help message
      -v --version             Print version information
