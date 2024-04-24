{ pkgs, ... }:

{
  env.LSP_USE_PLISTS="true";

  packages = with pkgs; [
    git
    zlib
    hpack
    universal-ctags
    haskellPackages.hoogle
    haskellPackages.ghcid
    haskellPackages.ormolu
    haskellPackages.hlint
    haskellPackages.implicit-hie
    haskellPackages.retrie
    haskellPackages.hasktags
    haskellPackages.haskdogs
    haskell.packages.ghc96.stack
    haskell.packages.ghc96.haskell-language-server
    haskell.packages.ghc96.hlint
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
