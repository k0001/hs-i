{
  description = "Haskell 'i' library";

  inputs = { hs_kind.url = "github:k0001/hs-kind"; };

  outputs = { self, nixpkgs, hs_kind }:
    let
      inherit (nixpkgs) lib;
      hs_i = import ./.;
      hspkgsOverrides = pself: psuper: hself: hsuper: {
        kind-integer = hsuper.callPackage hs_kind.hs_kind-integer { };
        i = hsuper.callPackage hs_i { };
      };
      pkgsOverlay = pself: psuper: {
        haskell = psuper.haskell // {
          packageOverrides = hspkgsOverrides pself psuper;
        };
      };
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ pkgsOverlay ];
        };

    in {
      inherit hs_i hspkgsOverrides pkgsOverlay;
      packages = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let pkgs = pkgsFor system;
          in {
            default = pkgs.releaseTools.aggregate {
              name = "every output from this flake";
              constituents = let
                p = self.packages.${system};
                s = self.devShells.${system};
              in [
                # p.hs_i__ghcDefault
                p.hs_i__ghc943

                # p.hs_i__ghcDefault.doc
                p.hs_i__ghc943.doc

                # s.hs_i__ghcDefault
                s.hs_i__ghc943
              ];
            };
            # hs_i__ghcDefault = pkgs.haskellPackages.i;
            hs_i__ghc943 = pkgs.haskell.packages.ghc943.i;
          });
      devShells = lib.genAttrs [ "x86_64-linux" "i686-linux" "aarch64-linux" ]
        (system:
          let
            pkgs = pkgsFor system;
            mkShellFor = hpkgs:
              hpkgs.shellFor {
                packages = p: [ p.i ];
                withHoogle = true;
                nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
              };
          in {
            default = self.devShells.${system}.hs_i__ghc943;
            # hs_i__ghcDefault = mkShellFor pkgs.haskellPackages;
            hs_i__ghc943 = mkShellFor pkgs.haskell.packages.ghc943;
          });
    };

}
