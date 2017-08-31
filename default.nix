{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./pump.nix { }
