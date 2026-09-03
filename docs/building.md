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

## Offline / locked-down machines

A box that can't reach Hackage — a corporate firewall stalls `cabal update`'s
hackage-security fetch even where a plain `curl` of the index succeeds — builds
against a **copied index** instead:

```
# on a machine that has a full index:
tar -C ~/.cache/cabal/packages -czf hackage-index.tgz hackage.haskell.org
# on the offline box (find its dir with `cabal path`, see 'cache-dir'):
tar -C <cache-dir>/packages -xzf hackage-index.tgz
cabal build --offline exe:glance        # --offline never phones home
```

`cabal.project` pins `index-state:` so a copied index and a freshly-updated one
resolve the **same** plan. **Bump that pin only when a new dependency needs a
package published after it — and in the same change re-copy the index to every
offline machine, or their `--offline` build resolves against the stale snapshot
and breaks.** No routine bumps.

## nix — reproducible, cross-platform

[`flake.nix`](../flake.nix) pins the same toolchain by hash, the companion to
`make bootstrap` for any machine with nix:

```
nix develop        # GHC, cabal, and (on Linux) the whole GTK/WebKitGTK stack
make install       # or: cabal build exe:glance, make browser-check, …
```

**macOS.** The server build always works. The native window is a **Cocoa/
WKWebView shim** (`src-desktop-native/cbits/glance_wkwebview.m`), a second
backend beside the Linux WebKitGTK one, chosen by OS at build time — WebKitGTK
itself is a Linux toolkit and does not run on a mac. `make install` builds it
(needs the Xcode **Command Line Tools** — `xcode-select --install` — for clang
and the Cocoa/WebKit frameworks; `nix develop` supplies them from the SDK). The
shim is **built and verified on Linux only so far**; the WKWebView window awaits
a build on a mac.

The design — a Cocoa/WKWebView backend behind the same two-function seam, and
the `.app`/codesign packaging still to come — is
[proposals/proposed/2026-09-02-a-darwin-window-cocoa-owns.md](proposals/proposed/2026-09-02-a-darwin-window-cocoa-owns.md).
