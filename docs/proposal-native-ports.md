# Proposal — native hosts: Linux window now, iOS/Android/WASM later

**Status:** draft · **Date:** 2026-08-05 · **Origin:** the native dive, with
the user's portability frame stated up front: iOS and Android ports later, a
WASM build possible.

## The strategy in one sentence

The PAGE is the app and every platform gets a THIN HOST for it — a webview
shim per OS plus a per-platform answer to "where does the daemon run" — so
porting never forks the UI, the grammar, or the write path.

## What today's tree already buys (verified, with the seams named)

- **The shell is host-agnostic.** One `location.host` reference (the socket
  line) and relative fetches everywhere else; nothing in the page names
  127.0.0.1. A host that serves the page from anywhere serves the whole app.
- **The transport is two helpers.** `getJSON`/`postJSON` (+ the socket open)
  are the page's entire wire surface — a different transport is a ~50-line
  adapter behind three names, not a rewrite.
- **The window is already an argument.** `Glance.Desktop.Native` takes the
  window as `String -> IO ()`; the GTK engine lives alone in the one
  flag-gated stanza (`glance-desktop-native`), and both flag states compile.
  A second platform's engine is a sibling stanza under the same rule: one
  flag, one stanza, every other component byte-identical.
- **The renderer is self-contained** (no CDN, no font, compiled in), and
  SCHEMA.md is the only contract between producer and page.
- **The nudge is accidentally WASM-shaped.** `Watch.writeSpans` = every write
  self-notifies through one queue door, because fsnotify could not be
  trusted with fresh directories. A platform with NO file watcher at all
  (WASM/OPFS, iOS sandboxes) runs the same architecture: writes nudge, a
  manual refresh nudges, the serial drain loop stays the sole store updater.

## The hosts

### 1. Linux window — NOW

WebKitGTK via the existing `make native`. Tonight's work: first real launch,
smoke on `DISPLAY=:0`, fix what surfaces. This host is also the template the
others copy: open at the socket, window-close stops the daemon
(`--keep-serving` restores stage 1), `_blank` goes to the system browser.

### 2. iOS — thin WKWebView host

A SwiftUI shell (~200 lines): one full-screen `WKWebView`, a settings screen
holding the daemon URL, `target="_blank"` routed to Safari (the `elsewhere`
rule, WebKit's own policy-decision API on both platforms). Daemon ladder:

- **v1 — remote daemon.** The Mac/home-server/tailnet daemon the desktop
  already runs; the phone is a client. Zero Haskell-on-iOS, ships first.
  Needs S7's auth tier before it leaves the tailnet.
- **v2 — the WASM core in the webview** (below): offline, on-device, no
  GHC-for-iOS toolchain.
- **v3 (maybe never) — on-device daemon.** GHC's aarch64-apple-ios cross
  exists but is heavy and App-Store-hostile; only worth it if WASM
  disappoints.

### 3. Android — thin WebView host, and the Termux shortcut

Same shim in Kotlin. Daemon ladder adds one rung iOS lacks: **the daemon is
a plain Linux binary**, so an aarch64-linux build runs under Termux TODAY —
host app points at `http://127.0.0.1:7777`, full offline app with the real
daemon, before any cross-compilation story. v1 remote, v1.5 Termux, v2 WASM.

### 4. WASM — the daemon inside the page

GHC's wasm32-wasi backend (9.10+ JSFFI) compiles the CORE — `glance-internal`
+ `Glance.Query` — into the page itself:

- A new consumer `glance-wasm` beside `glance-web`, under the same law:
  depends on the public library alone. It exposes load/query/command/
  materialize as JSFFI exports; the facade boundary IS the port seam.
- The shell's transport adapter routes `getJSON`/`postJSON` to module calls
  and the socket to a callback queue; the page above the adapter does not
  change.
- The tree lives in OPFS (import/export a directory; File System Access API
  where the browser grants it). No watcher exists — which the nudge
  architecture already assumes; an explicit refresh button is the config
  reseed with a face.
- What stays behind: the process-wide walk numbers (OPFS I/O differs), the
  native window (the browser is the window), `edit-link`'s symlink caveat
  (no symlinks in OPFS).

## Portability rules to adopt now (cheap, and this doc is their home)

1. The page's wire surface stays `getJSON`/`postJSON`/one socket open —
   a new fetch call goes through them or it does not merge.
2. Nothing in the shell assumes POSIX paths; row ids and file names are
   opaque strings end to end (true today — keep it).
3. A platform host is one flag + one stanza + one engine module taking the
   window as a function — `glance-desktop-native` is the template.
4. The daemon-URL question is the HOST's, never the page's: the page asks
   `location.host` and nothing else.

## First steps, in order

1. Tonight: `make native` smoke on the live display; fix; record.
2. `aarch64-unknown-linux` release build target in the Makefile (the Termux
   rung — costs a cross toolchain or a device build, enables Android v1.5).
3. A `transport.js`-shaped extraction is NOT owed yet (rule 1 already holds);
   revisit when `glance-wasm` starts.
4. WASM spike after the GHC toolchain lands locally: compile
   `glance-internal` alone, measure size, before any JSFFI design.
