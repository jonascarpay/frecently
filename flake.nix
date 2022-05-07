{
  description = "frecently";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              frecently = hfinal.callCabal2nix "frecently" ./. { };
            };
        };

        frecently = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.frecently;

        frecently-shell = final.haskellPackages.shellFor {
          withHoogle = false;
          packages = hpkgs: [ hpkgs.frecently ];
          nativeBuildInputs = [
            final.cabal-install
            final.ghcid
            final.haskellPackages.haskell-language-server
            final.hlint
            final.purescript
            final.ormolu
            final.spago
            final.bashInteractive # see: https://discourse.nixos.org/t/interactive-bash-with-nix-develop-flake/15486
          ];
        };
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
        in
        {
          defaultPackage = pkgs.frecently;
          packages.frecently = pkgs.frecently;
          packages.frecently-static = pkgs.pkgsStatic.frecently;
          packages.frecently-aarch64-static = pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic.frecently;
          devShell = pkgs.frecently-shell;
          checks.integration-tests = import ./test pkgs;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
