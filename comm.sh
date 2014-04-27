#!/bin/bash
file=$1
sel=$2

path=$(grep "^$sel" $file | head -1 | awk '{print $NF}')
echo $path
url=https://news.ycombinator.com/$path
echo $url >&2
curl $url  | tidy -utf8 -wrap 0 2>/dev/null  | .cabal-sandbox/bin/hncomment | fmt 80 | less 

