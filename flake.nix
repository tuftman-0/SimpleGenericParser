{
  description = "A simple generic parser library for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        haskellPackages = pkgs.haskellPackages;
      in
      {
        packages.default = haskellPackages.callCabal2nix "SimpleGenericParser" ./. {};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
          ];
        };
      }
    );
}
