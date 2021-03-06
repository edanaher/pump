{ mkDerivation, base, hslua, digest, HUnit, stdenv, lens, docopt, syb }:
mkDerivation {
  pname = "pump";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  testHaskellDepends = [ HUnit ];
  executableHaskellDepends = [ base hslua digest lens docopt syb ];
  description = "Compiler targeting deflate compressed streams (gzip, zip, etc.)";
  license = stdenv.lib.licenses.bsd3;
  #doCheck = false;
  postInstall =
    ''
      mkdir $out/lua
      cp lua/dsl.lua $out/lua
    '';
  testTarget = ''--show-details=direct -v'';

  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
}
