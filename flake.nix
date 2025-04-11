{
  description = "Flake with environment variables and installed packages";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems f;
    in {
      devShells = forAllSystems (system: let
        pkgs = import nixpkgs { inherit system; };
      in {
        default = pkgs.mkShell {
          packages = with pkgs; [
            zlib
            fzf
            fd
            jq
            ripgrep
            git
            pre-commit
            universal-ctags

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

            nodePackages.bash-language-server
          ];

          shellHook = ''
            export DEVENV_ROOT=$(git rev-parse --show-toplevel 2>/dev/null || echo "$PWD")
            export FZF_DEFAULT_COMMAND="fd --type f --strip-cwd-prefix"
            export PROJECT_NAME=$(basename "$DEVENV_ROOT")-$PYTHON_VERSION

            function wake() {
                set -e

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
            }

            export -f wake
          '';
        };
      });
    };
}
