{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs; [
    git
    haskellPackages.fourmolu
    haskellPackages.cabal-fmt
    hlint
    nixfmt-rfc-style
    treefmt
    yamlfmt
  ];

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo Hello from $GREET";

  enterShell = ''
    hello
    git --version
  '';

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep "2.42.0"
  '';

  # https://devenv.sh/languages/
  languages = {
    nix.enable = true;
    haskell = {
      enable = true;
      # package = pkgs.haskell.compiler.ghc98; # Needs cabal >= 3.10.1.2
    };
  };

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks = {
    hlint.enable = true;
    treefmt.enable = true;
  };

  # See full reference at https://devenv.sh/reference/options/
}
