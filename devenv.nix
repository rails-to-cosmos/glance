{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  packages = with pkgs; [
    git
    zlib
    hpack
    stack
    haskellPackages.hoogle
    haskellPackages.hasktags
    haskellPackages.haskdogs
    haskell.compiler.ghc945
    haskell.packages.ghc945.haskell-language-server
    haskell.packages.ghc945.hlint
  ];

  # https://devenv.sh/scripts/
  scripts.run-tests.exec = ''
    hlint src --report
    stack test
  '';

  enterShell = ''
    echo "Hello fellow hacker"
    ghc --version
  '';

  # https://devenv.sh/languages/
  # languages.nix.enable = true;

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
