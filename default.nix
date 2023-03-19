{ mkDerivation, base, constraints, hedgehog, kind-integer, kind-rational, lib
, tasty-hedgehog, tasty-hunit, tasty }:
mkDerivation {
  pname = "i";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  libraryHaskellDepends = [ base constraints kind-integer kind-rational ];
  testHaskellDepends = [ base hedgehog tasty-hedgehog tasty-hunit tasty ];
  homepage = "https://github.com/k0001/hs-i";
  description = "Haskell interval types";
  license = lib.licenses.asl20;
}
