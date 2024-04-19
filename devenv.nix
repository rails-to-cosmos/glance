{ pkgs, ... }:

{
  # https://devenv.sh/basics/

  env.LSP_USE_PLISTS="true";

  packages = with pkgs; [
    git
    zlib
    hpack
    stack
    universal-ctags
    haskellPackages.hoogle
    haskellPackages.ghcid
    haskellPackages.ormolu
    haskellPackages.hlint
    haskellPackages.implicit-hie
    haskellPackages.retrie
    haskellPackages.hasktags
    haskellPackages.haskdogs
    haskell.compiler.ghc945
    haskell.packages.ghc945.haskell-language-server
    haskell.packages.ghc945.hlint
  ];

  scripts.wake.exec = ''
    haskdogs --deps-dir ".haskdogs" --use-stack ON
  '';

  enterShell = ''
    echo "Hello fellow hacker"
    ghc --version
  '';

  enterTest = ''
    hlint src --report
    stack test
  '';

}
