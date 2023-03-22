{ mkDerivation, base, Cabal, constraints, directory, hedgehog, kind-integer
, kind-rational, lib, tasty, tasty-hedgehog, tasty-hunit }:
mkDerivation {
  pname = "i";
  version = "0.1";
  src = lib.sources.cleanSource ./.;
  setupHaskellDepends = [ base Cabal directory ];
  libraryHaskellDepends = [ base constraints kind-integer kind-rational ];
  testHaskellDepends = [
    base
    constraints
    hedgehog
    kind-integer
    kind-rational
    tasty
    tasty-hedgehog
    tasty-hunit
  ];
  homepage = "https://github.com/k0001/hs-i";
  description = "Haskell interval types. Bounds checking.";
  license = lib.licenses.asl20;
}
