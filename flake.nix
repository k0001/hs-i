{
  description = "Haskell 'i' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs?rev=21eda9bc80bef824a037582b1e5a43ba74e92daa";
    flake-parts.url = "github:hercules-ci/flake-parts";
    hs_kind.url = "github:k0001/hs-kind";
    hs_kind.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default =
        inputs.nixpkgs.lib.composeExtensions inputs.hs_kind.overlays.default
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = prev.lib.composeExtensions
              (prev.haskell.packageOverrides or (_: _: { }))
              (hself: hsuper: { i = hself.callPackage ./. { }; });
          };
        });
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          i__ghc943 = pkgs.haskell.packages.ghc943.i;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.i__ghc943
              config.packages.i__ghc943.doc
              config.devShells.ghc943
            ];
          };
        };
        devShells = {
          default = config.devShells.ghc943;
          ghc943 = pkgs.haskell.packages.ghc943.shellFor {
            packages = p: [ p.i ];
            withHoogle = true;
            nativeBuildInputs = [ pkgs.cabal-install pkgs.cabal2nix ];
          };
        };
      };
    };
}
