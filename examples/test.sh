#!/bin/sh

set -e

TOPDIR=$(dirname $(dirname $PWD/$0))
PUMP=$TOPDIR/result/bin/pump

$PUMP $TOPDIR/examples/narcissus.lua -s 
MD5=$(md5sum $TOPDIR/examples/narcissus.gz)
mv narcissus.gz a.gz
gunzip -N a.gz
MD5b=$(md5sum $TOPDIR/examples/narcissus.gz)
diff <(echo $MD5) <(echo $MD5b)
diff $TOPDIR/examples/narcissus.in $TOPDIR/examples/narcissus.out

echo "Narcissus successful"
rm $TOPDIR/examples/narcissus{.in,.out,.gz}

$PUMP $TOPDIR/examples/tweedledee.lua
MD5=$(md5sum $TOPDIR/examples/tweedledee.gz)
gunzip -N tweedledee.gz
gunzip -N tweedledum.gz
MD5b=$(md5sum $TOPDIR/examples/tweedledee.gz)
diff <(echo $MD5) <(echo $MD5b)
rm $TOPDIR/examples/tweedledee.gz

echo "Tweedledee successful"
