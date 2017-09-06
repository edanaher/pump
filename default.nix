let pkgs = import <nixpkgs> {};
    stdenv = pkgs.stdenv;
    my-haskell-pkgs = pkgs.haskell.packages.ghc802.extend (self: super: {
      hslua_0_8_0 = super.hslua_0_8_0.override {
        lua5_1 = pkgs.lua5_3;
      };
    });
in
my-haskell-pkgs.callPackage ./pump.nix { }
