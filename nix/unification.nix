{ mkDerivation, base, deriving-compat, equivalence, fetchgit, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "unification";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/unification";
    sha256 = "0zkb2p5c2rxcl4mc9hm3x98734xp9nv1ic73h2pij26ncamc36mj";
    rev = "d12603fe87df06f252a667356a5d6ed216561d38";
  };
  libraryHaskellDepends = [
    base deriving-compat equivalence lens mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
