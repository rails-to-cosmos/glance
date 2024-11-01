{ pkgs, ... }:

{
  packages = with pkgs; [
    fzf
    fd
    ripgrep

    git
    pre-commit
    universal-ctags

    # haskell.compiler.ghc964
    # haskell.packages.ghc964.hpack
    # haskell.packages.ghc964.stack
    # haskell.packages.ghc964.ghcid
    # haskell.packages.ghc964.haskdogs
    # haskell.packages.ghc964.haskell-language-server
    # haskell.packages.ghc964.hasktags
    # haskell.packages.ghc964.hlint
    # haskell.packages.ghc964.hoogle
    # haskell.packages.ghc964.implicit-hie
    # haskell.packages.ghc964.ormolu
    # haskell.packages.ghc964.retrie
  ];

  scripts.wake.exec = ''
    ghcup upgrade
    ghcup install ghc $GHC_VERSION
    ghcup set ghc $GHC_VERSION

    ghcup install cabal $CABAL_VERSION
    cabal update

    # haskdogs --deps-dir lib --use-stack ON --hasktags-args -e
  '';

  enterShell = ''
    echo "Hello fellow hacker"
  '';

  enterTest = ''
    hlint src --report
    stack test
  '';

  scripts.run-test.exec = ''
    stack test --test-arguments "--pattern \"$@\""
  '';

}
