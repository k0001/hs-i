{ mkDerivation, constraints, lib }:
mkDerivation {
  pname = "i";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [ constraints ];
  homepage = "https://github.com/k0001/hs-i";
  description = "Haskell interval types";
  license = lib.licenses.asl20;
}
