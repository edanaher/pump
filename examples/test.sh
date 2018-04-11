#!/usr/bin/env bash

set -e

TOPDIR=$(dirname $(dirname $PWD/$0))
PUMP=$TOPDIR/result/bin/pump
EXAMPLES=$TOPDIR/examples

if [[ "$1" == narcissus || "$1" == "" ]]; then
  $PUMP $EXAMPLES/narcissus.lua -s
  MD5=$(md5sum $EXAMPLES/narcissus.gz)
  mv $EXAMPLES/narcissus.gz $EXAMPLES/a.gz
  gunzip -N $EXAMPLES/a.gz
  MD5b=$(md5sum $EXAMPLES/narcissus.gz)
  diff -u <(echo $MD5) <(echo $MD5b)
  diff -u $EXAMPLES/narcissus.in $EXAMPLES/narcissus.out

  echo "Narcissus successful"
  rm $EXAMPLES/narcissus{.in,.out,.gz}
fi

if [[ "$1" == tweedledee || "$1" == "" ]]; then
  $PUMP $EXAMPLES/tweedledee.lua
  MD5=$(md5sum $EXAMPLES/tweedledee.gz)
  gunzip -N $EXAMPLES/tweedledee.gz
  gunzip -N $EXAMPLES/tweedledum.gz
  MD5b=$(md5sum $EXAMPLES/tweedledee.gz)
  diff -u <(echo $MD5) <(echo $MD5b)
  rm $EXAMPLES/tweedledee.gz

  echo "Tweedledee successful"
fi
