{ mkDerivation, base, containers, lens, stdenv, unification }:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers lens unification ];
  license = stdenv.lib.licenses.bsd3;
}
