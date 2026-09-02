# Building & installing

**Status:** doctrine · **Date:** 2026-09-02

One prerequisite, [`ghcup`](https://www.haskell.org/ghcup/). Everything else the
build needs is bootstrapped from it.

## `make install` (Arch / CachyOS)

```
make install
```

`tools/bootstrap` runs first and, idempotently:

- installs the **pinned GHC** (`cabal.project`'s `with-compiler:`, read from that
  line so the two never drift) and **cabal** via `ghcup`, if missing;
- installs the **WebKitGTK system libraries** the desktop window links — `gtk3`,
  `webkit2gtk-4.1`, `gobject-introspection`, `pkgconf` — via `sudo pacman -S
  --needed`, and only the ones `pkg-config` reports missing. A satisfied machine
  installs nothing.

Then the native window is built (`cabal.project.native` + the vendored bindings)
and the binary is copied to `~/.local/bin/glance`.

`pacman` is the one package manager the bootstrap drives. On another distro it
names what is missing and stops; install the equivalents by hand, or use nix
below.

## Server build only — anywhere

The web/server half is pure Haskell and links **no** system libraries:

```
cabal build exe:glance      # ghc + cabal only
```

Portable to any machine with the toolchain — macOS included.

## nix — reproducible, cross-platform

[`flake.nix`](../flake.nix) pins the same toolchain by hash, the companion to
`make bootstrap` for any machine with nix:

```
nix develop        # GHC, cabal, and (on Linux) the whole GTK/WebKitGTK stack
make install       # or: cabal build exe:glance, make browser-check, …
```

**macOS.** The server build works. The native window does **not**: it is
WebKitGTK, a Linux toolkit, and nixpkgs `webkitgtk` on Darwin is commonly
broken. A real mac app needs a **WKWebView (Cocoa) shell** — a separate delivery
alongside the WebKitGTK one, not yet built — so nix cannot conjure it. On a mac,
use nix for the server build and treat the desktop window as Linux-only.
