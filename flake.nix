{
  description = "glance -- an org viewer (Haskell + Elm + vanilla JS), with its WebKitGTK desktop window";

  # A REPRODUCIBLE DEV ENV, the cross-platform companion to `make bootstrap'.
  # `make bootstrap' is the one-command path on Arch (ghcup + pacman); this is
  # the same toolchain pinned by hash, for any machine that has nix.  Enter with
  # `nix develop', then `make', `make install', `make browser-check' -- the
  # shell carries GHC, cabal, and (on Linux) the whole GTK/WebKitGTK stack.
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;

        # The compiler glance PINS (cabal.project `with-compiler: ghc-9.6.7').
        # Keep this in step with that line -- `tools/bootstrap' reads it from
        # cabal.project; here it is spelled once as the nixpkgs attribute.
        ghc = pkgs.haskell.compiler.ghc967;

        # The WebKitGTK stack the NATIVE window links.  Linux only: WebKitGTK is
        # a Linux toolkit and nixpkgs `webkitgtk' on Darwin is frequently marked
        # broken, so a mac gets the server toolchain and skips this -- the plain
        # `cabal build exe:glance' / `glance serve' needs none of it.  A native
        # mac window is a separate WKWebView shell, not a packaging choice.
        #
        # `webkitgtk_4_1' is the libsoup3 generation carrying `webkit2gtk-4.1.pc'
        # and `javascriptcoregtk-4.1.pc' -- the exact modules the vendored
        # bindings in `cabal.project.native' target, so `make native' resolves
        # against it without fighting the host over which WebKitGTK is present.
        nativeLibs = with pkgs; lib.optionals stdenv.isLinux [
          gtk3
          webkitgtk_4_1
          glib
          cairo
          pango
          gdk-pixbuf
          harfbuzz
          at-spi2-core
        ];
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            ghc
            cabal-install
            pkg-config
            gobject-introspection   # g-ir tooling + system GIRs / typelibs
            nodejs_22               # the browser + interop drivers
          ];
          buildInputs = nativeLibs;

          shellHook = ''
            echo "glance dev shell -- GHC ${ghc.version}, $(cabal --version | head -1)"
          '' + lib.optionalString (!pkgs.stdenv.isLinux) ''
            echo "note: on Darwin the server build works; the native WebKitGTK window does not -- it needs a WKWebView shell that is not built."
          '';
        };
      });
}
