#!/bin/sh
echo :quit | cabal-dev ghci 2>/dev/null | sed -n 's/^Loading package //p' | sed 's/ \.\.\. linking \.\.\. done\.$//;s/-\([0-9\.]\+\)$/ == \1/;1s/^/  /;2,$s/^/, /'
