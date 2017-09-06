{ mkDerivation, base, hslua_0_8_0, stdenv }:
mkDerivation {
  pname = "pump";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hslua_0_8_0 ];
  description = "Compiler targeting deflate compressed streams (gzip, zip, etc.)";
  license = stdenv.lib.licenses.bsd3;
  postInstall =
    ''
      mkdir $out/lua
      cp lua/dsl.lua $out/lua
    '';
}
