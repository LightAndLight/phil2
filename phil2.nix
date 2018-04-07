{ mkDerivation, base, containers, deriving-compat, lens, mtl
, stdenv, unification
}:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deriving-compat lens mtl unification
  ];
  license = stdenv.lib.licenses.bsd3;
}
