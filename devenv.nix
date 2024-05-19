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

  # Update third-party dependencies and add it to stack.yaml
  scripts.package.exec = ''
    haskdogs --deps-dir lib --use-stack ON --hasktags-args -e

    directories=$(find ./lib -mindepth 1 -maxdepth 1 -type d)

    deps_list="extra-deps\n  - ."
    for dir in $directories;
    do
        # Remove leading './' from directory path
        relative_dir=$(echo "$dir" | sed 's|^\./||')
        deps_list="$deps_list\n  - $relative_dir"
    done

    # Update stack.yaml
    # First, create a backup of the original stack.yaml
    cp stack.yaml stack.yaml.bak

    # Use awk to replace the existing deps section with the new one
    awk -v new_deps="$deps_list" '
        BEGIN { in_deps = 0 }
        /^extra-deps/ { in_deps = 1; print new_deps; next }
        in_deps && /^\s*-/ { next }
        !in_deps || !/^\s*$/ { in_deps = 0; print }
    ' stack.yaml.bak > stack.yaml

    echo "Updated stack.yaml with the following deps list:"
    echo -e "$deps_list"
  '';

  enterShell = ''
    echo "Hello fellow hacker"
    ghc --version
  '';

  enterTest = ''
    hlint src --report
    stack test
  '';

  scripts.run-test.exec = ''
    stack test --test-arguments "--pattern \"$@\""
  '';

}
