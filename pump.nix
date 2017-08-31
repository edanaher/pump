{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "pump";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Compiler targeting deflate compressed streasm (gzip, zip, etc.)";
  license = stdenv.lib.licenses.bsd3;
}
