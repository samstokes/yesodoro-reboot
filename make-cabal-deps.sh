#!/bin/sh
cabal-dev buildopts | grep -A1 -- -package-id | grep -v '^-' | grep -v yesodoro-reboot | sort -u | sed 's/-[0-9a-f]\+$//;s/-\([0-9\.]\+\)$/ == \1/;1s/^/  /;2,$s/^/, /'
