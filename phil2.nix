{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
