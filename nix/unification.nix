{ mkDerivation, base, deriving-compat, equivalence, fetchgit, lens
, mtl, stdenv
}:
mkDerivation {
  pname = "unification";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/unification";
    sha256 = "0117bd7gdgyfj10qgpqlfmxdrm655af91l5glb5qmnaqi8v1qccv";
    rev = "845614f5ced32ec72f22a2d0b2b739cc600122d7";
  };
  libraryHaskellDepends = [
    base deriving-compat equivalence lens mtl
  ];
  license = stdenv.lib.licenses.bsd3;
}
