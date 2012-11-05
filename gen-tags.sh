#!/bin/sh
echo :ctags | ghci $(cabal-dev buildopts | egrep -v -- '--make|==|-threaded') -v0
