#!/bin/sh
cabal v2-build
~/haskell/newton-polygon/dist-newstyle/build/x86_64-linux/ghc-8.8.4/newton-polygon-0.1.0.0/x/test/build/test/test > output.txt
