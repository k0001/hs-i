{ mkDerivation, constraints, base, kind-integer, kind-rational, lib }:
mkDerivation {
  pname = "i";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ constraints kind-integer kind-rational base ];
  homepage = "https://github.com/k0001/hs-i";
  description = "Haskell interval types";
  license = lib.licenses.asl20;
}
