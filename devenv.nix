{ pkgs, ... }:

{
  env.LSP_USE_PLISTS = "true";

  packages = with pkgs; [
    git
    zlib
    universal-ctags
    ripgrep

    haskell.compiler.ghc964
    haskell.packages.ghc964.hpack
    haskell.packages.ghc964.stack
    haskell.packages.ghc964.ghcid
    haskell.packages.ghc964.haskdogs
    haskell.packages.ghc964.haskell-language-server
    haskell.packages.ghc964.hasktags
    haskell.packages.ghc964.hlint
    haskell.packages.ghc964.hoogle
    haskell.packages.ghc964.implicit-hie
    haskell.packages.ghc964.ormolu
    haskell.packages.ghc964.retrie
  ];

  scripts.init.exec = ''
    haskdogs --deps-dir "lib" --use-stack ON
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
