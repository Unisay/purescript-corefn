{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    let
      supportedSystems = [ "x86_64-linux" ];
    in
    flake-utils.lib.eachSystem supportedSystems (
      system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        overlays = [
          haskellNix.overlay
          (final: prev: {
            project = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc98";
              evalSystem = "x86_64-linux";
              modules =
                let
                  prof = false;
                in
                [
                  {
                    doHaddock = false;
                    doHoogle = false;
                    enableProfiling = prof;
                    enableLibraryProfiling = prof;
                  }
                ];

              name = "purescript-corefn";

              shell = {
                tools = {
                  cabal = { };
                  cabal-fmt = { };
                  fourmolu = { };
                  hlint = { };
                  haskell-language-server = { };
                };
                buildInputs = with pkgs; [
                  treefmt
                  yamlfmt
                ];
              };

              crossPlatforms =
                p:
                pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 (
                  pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [ p.musl64 ]
                );
            };
          })
        ];
        flake = pkgs.project.flake { };
      in
      flake
    );

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
