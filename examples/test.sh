#!/usr/bin/env bash

set -e

TOPDIR=$(dirname $(dirname $PWD/$0))
PUMP=$TOPDIR/result/bin/pump
EXAMPLES=$TOPDIR/examples

$PUMP $EXAMPLES/narcissus.lua -s 
MD5=$(md5sum $EXAMPLES/narcissus.gz)
mv $EXAMPLES/narcissus.gz $EXAMPLES/a.gz
gunzip -N $EXAMPLES/a.gz
MD5b=$(md5sum $EXAMPLES/narcissus.gz)
diff <(echo $MD5) <(echo $MD5b)
diff $EXAMPLES/narcissus.in $EXAMPLES/narcissus.out

echo "Narcissus successful"
rm $EXAMPLES/narcissus{.in,.out,.gz}

$PUMP $EXAMPLES/tweedledee.lua
MD5=$(md5sum $EXAMPLES/tweedledee.gz)
gunzip -N $EXAMPLES/tweedledee.gz
gunzip -N $EXAMPLES/tweedledum.gz
MD5b=$(md5sum $EXAMPLES/tweedledee.gz)
diff <(echo $MD5) <(echo $MD5b)
rm $EXAMPLES/tweedledee.gz

echo "Tweedledee successful"
