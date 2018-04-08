{ mkDerivation, base, bytestring, containers, deriving-compat, lens
, mtl, parsers, stdenv, trifecta, unification
}:
mkDerivation {
  pname = "phil2";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers deriving-compat lens mtl parsers
    trifecta unification
  ];
  license = stdenv.lib.licenses.bsd3;
}
