Pump
====

A "compiler" targetting deflate-encoded streams like gzip and zip files.  Using a quine-like programming model, it's possible to do simple computations; a previous iteration could count to a billion by repeated extraction: bil000000000.gz extracted to bil000000001.gz, and so on until wrapping around to zero after a billion extractions.

I suspect that this repeated unzipping is Turing complete, but programming (and debugging) in this model is a pain.  This is an attempt to build a compiler that not only produces a .gz or .zip file, but can also simulate a program at a higher level to make development easer.

Status
------
Currently the examples generally work, though simulating is still a work in progress.  A number of extra feature will be needed for more advanced programs (e.g., zeros on multiple ranges, forcing blocks to be the same size for large-scale substitutions), but the goal is to build back up to that with working simulations.

Examples
--------
examples/narcissus.lua produces a gzip quine; extracting the file produces itself.

examples/tweedledee.lua produces a "double quine"; tweedledee.gz decompresses to tweedledum.gz, which in turn decompresses back to tweedledee.gz.


Development
-----------

I use nix; with it, building and running is as easy as

```
$ nix-build
$ result/bin/pump examples/narcissus.gz
$ result/bin/pump examples/tweedledee.lua -o tweedledee.gz # Ignore the copious debug output
$ gunzip -N tweedledee.gz
$ gunzip -N tweedledum.gz
$ gunzip -N tweedledee.gz
$ gunzip -N tweedledum.gz
# Repeat until bored
```
