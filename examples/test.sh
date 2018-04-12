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

  $PUMP $EXAMPLES/narcissus.out -o $EXAMPLES/narc.gz
  diff -u $EXAMPLES/narcissus.gz $EXAMPLES/narc.gz

  echo "Narcissus successful"
  rm $EXAMPLES/narcissus{.in,.out,.gz} $EXAMPLES/narc.gz
fi

if [[ "$1" == tweedledee || "$1" == "" ]]; then
  $PUMP -s $EXAMPLES/tweedledee.lua -s
  MD5=$(md5sum $EXAMPLES/tweedledee.gz)
  rm $EXAMPLES/tweedledum.gz || true
  gunzip -N $EXAMPLES/tweedledee.gz
  MD5dum=$(md5sum $EXAMPLES/tweedledum.gz)
  gunzip -N $EXAMPLES/tweedledum.gz
  MD5b=$(md5sum $EXAMPLES/tweedledee.gz)
  diff -u <(echo $MD5) <(echo $MD5b)

  #$PUMP $EXAMPLES/tweedledee.out -o $EXAMPLES/tweedledum.gz
  #MD5dumb=$(md5sum $EXAMPLES/tweedledum.gz)
  #diff -u <(echo $MD5dum) <(echo $MD5dumb)

  #rm $EXAMPLES/tweedledee.{in,out} tweedledum.gz
  rm $EXAMPLES/tweedledee.gz

  echo "Tweedledee successful"
fi
