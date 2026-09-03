# Proposal — a Darwin native window, Cocoa/WKWebView behind the same seam

**Status:** proposed · **Date:** 2026-09-02 · **Source:** the "`make install`
should only need ghcup / how does this run on osx" thread — `tools/bootstrap`
now stops cleanly off-Linux (`5cc1dd9`) because the native window is
WebKitGTK, and this is the design for the window a mac gets instead.

## Background

`glance desktop` opens a window this process owns: a webview filling it,
loading the localhost URL the same binary serves. Today that window is
**WebKitGTK** — GTK 3 + `gi-webkit2` — a Linux toolkit. On macOS it does not
run: WebKitGTK is X11/Wayland, and `nixpkgs.webkitgtk` on Darwin is commonly
broken. A mac needs the platform's own engine, **WKWebView** (WebKit.framework
+ Cocoa), which is the same WebKit under a Cocoa surface.

The server half is already portable — `cabal build exe:glance` / `glance serve`
links no system libraries and runs anywhere. Only the WINDOW is Linux-bound.

## The seam is already tiny

The window's whole contract is **two exports** from
`src-desktop-native/Glance/Desktop/WebKit.hs`:

```haskell
nativeAvailable :: Bool
nativeWindow    :: (Int, Int) -> String -> String -> IO ()   -- (zoom band) title url; blocks till close
```

`src-web/Glance/Desktop/Native.hs` consumes them through an injected
`String -> IO ()` (`desktopWith`, `runNative`), so the daemon/policy/reporting
layer is already engine-agnostic and the suite drives it with a **fake window**
(`desktopWith available window …`). Everything GTK — the decide-policy handler,
the `window.open` postback, the popup window, zoom, the black paint — is
PRIVATE to that module.

A mac backend is therefore additive: a second module offering the same two
names, chosen by OS at build time. **The Linux path is not touched.**

## Proposal

A second backend behind the seam: a small **Objective-C shim** driving WKWebView
+ Cocoa, selected by `if os(osx)` in the cabal file, gated by the same
`native-window` flag.

### The capability map

Every GTK piece has a clean Cocoa equivalent; the crux is that the JS↔native
bridge is WebKit-standard, so **the page's own JS is unchanged**.

| WebKitGTK today (`WebKit.hs`) | WKWebView on Darwin |
|---|---|
| `GtkWindow` + `WebKitWebView`, 1200×800, title | `NSWindow` + `WKWebView` |
| black `window{background:#000}`, `webViewSetBackgroundColor` | `NSWindow.backgroundColor` black, `WKWebView` non-opaque |
| `window.webkit.messageHandlers.{popup,quit,zoom}.postMessage` | **identical** — WKWebView is WebKit too |
| `openOverride` `UserScript` at injection-time-start | `WKUserScript` at `.atDocumentStart` — same JS |
| decide-policy: `_blank`/`window.open` → popup or system browser | `WKNavigationDelegate` + `WKUIDelegate.createWebViewWith…` + `NSWorkspace.open` |
| transient popup window, ESC closes, in-place nav | second `NSWindow` + `WKWebView`, `keyDown` ESC |
| `webViewSetZoomLevel` | `WKWebView.pageZoom` |
| `sigINT` → `quitLoop`; `gtk_main` on the main thread | signal → `[NSApp terminate:]`; `[NSApp run]` on the main thread |

The one contract that must NOT drift is the bridge: the injected
`window.open = … messageHandlers.popup.postMessage(String(u))` and the
`quit`/`zoom` handler names are `window.webkit.messageHandlers.*`, byte-identical
on WKWebView. So the page-side script (`openOverride` in `WebKit.hs`) is reused
verbatim; only the native RECEIVER differs.

### The shape

- `src-desktop-native/cbits/glance_wkwebview.m` — Objective-C. One exported C
  entry, `void glance_native_window(int zmin, int zmax, const char *title, const
  char *url)`, that **blocks until the window closes**, matching `nativeWindow`'s
  `IO ()` contract. It builds `NSApp`/`NSWindow`/`WKWebView` with a
  `WKUserContentController` (handlers `popup`/`quit`/`zoom`), the injected
  override script, a navigation/UI delegate for the link policy, black
  background, `pageZoom`, and `[NSApp run]`. **All three messages are handled
  inside the ObjC** (open → `NSWorkspace`/popup, quit → close, zoom →
  `pageZoom`), so the FFI boundary is ONE call with no Haskell callbacks.
- A Darwin branch (a `#ifdef darwin_HOST_OS` in `WebKit.hs`, or a sibling
  `Glance.Desktop.WKWebView`) exports `nativeAvailable = True` and
  `nativeWindow band title url = c_glance_native_window …` via `foreign import
  ccall`. `zoomAsked` is pure and already shared — no second copy.
- `glance.cabal`, the `glance-desktop-native` library: `if os(osx)` adds
  `c-sources: src-desktop-native/cbits/glance_wkwebview.m` and `frameworks:
  Cocoa WebKit`; `if os(linux)` keeps the `gi-*` build-depends. The
  `native-window` flag gates both, so a flagless build is base-only on every OS,
  as it is now.
- `nativeWindowLine` (`Native.hs`) branches its label — `(WebKitGTK)` on Linux,
  `(WKWebView)` on Darwin — so the dry-run/report never claims the wrong engine.
- `tools/bootstrap` off-Linux: stop DYING; ensure the ghcup toolchain and check
  `xcode-select -p` (the Command Line Tools carry the ObjC compiler + the
  Cocoa/WebKit frameworks — nothing to package-install).
- `flake.nix`: the Darwin devShell adds the SDK frameworks (`apple-sdk` /
  `darwin.apple_sdk.frameworks.{Cocoa,WebKit}`) and drops its "not supported"
  note; `webkitgtk_4_1` stays Linux-gated.

### From window to `.app` (second stage)

A window is not yet an app a mac double-clicks. A follow-on, out of this
proposal's core but named so it is not forgotten: wrap the binary in a `.app`
bundle (an `Info.plist`, an icon, the `glance` binary as the executable that
`serve`s and opens the window), then **codesign** and **notarize** it for
distribution outside a terminal. `make dist` grows a `x86_64-darwin`/`aarch64-darwin`
folder holding the `.app`, honest to what the building machine produced, the
same rule the per-triple bundle already keeps.

## Why a second backend, not the `webview` C library

The `webview/webview` lib is one API over WKWebView (mac), WebKitGTK (Linux) and
WebView2 (Windows) — tempting as a single replacement. Rejected:

- Its API is minimal and does **not** expose the navigation policy (external
  link → system browser vs in-app popup), the per-window black background, or
  the zoom the page drives — all of which this window relies on.
- Adopting it would REPLACE the tuned, working Linux path (the vendored
  `gi-webkit2` 4.1 bindings, the zoom band, the black paint) with a less
  controllable one — a regression on the platform that already works.

The seam is two functions; a parallel Cocoa backend is additive and **zero-risk
to Linux**, which the whole-lib swap is not.

## Verification without a mac on the desk

The port splits into three layers; two are testable OFF a mac, isolating the
Cocoa-only part.

1. **Shared bridge/policy logic** — the `window.open` override, the
   `popup`/`quit`/`zoom` names, the "webby → popup, else system-open" rule. This
   is WebKit-standard and already runs on the WebKitGTK backend, which **podman
   can run headlessly** (a Linux container + `Xvfb`/`weston --headless`). The
   repo already drives podman for the Emacs `interop` target
   (`EMACS_RUN=podman`), so this fits that workflow.
2. **The Haskell FFI + cabal wiring** — compile-check `foreign import ccall
   glance_native_window` and the `if os(osx)` stanza in a Linux container against
   a **stub `glance_native_window.c`** (same symbol, no-op body). Proves the seam
   builds and links; only the ObjC BODY stays mac-only.
3. **The Cocoa body** — needs a real mac. **A `macos-14` GitHub Actions runner**
   (Apple silicon, real Xcode + SDK) builds the shim, links WebKit, and runs a
   headless launch-and-quit smoke with a screenshot; alternatives are a `tart`
   macOS VM (Apple-silicon host) or a cloud mac. **podman cannot do this layer**:
   no macOS in a Linux container (Apple's SDK + a running WindowServer are
   required, and the QEMU-macOS-in-a-container trick is EULA-gray and impractical
   for GUI).

## Trade-offs & risks

- **Build ON a mac.** ObjC + Apple frameworks means no cross-compile from Linux;
  the shim is built and hand-verified on macOS (or the `macos-14` runner),
  exactly as the GTK window is hand-verified on Linux.
- **A new language in the tree.** ~200–400 lines of Objective-C. Contained to one
  `cbits/*.m`; the Haskell side is ~30 lines of FFI + cabal.
- **Two window implementations to keep in step.** The seam (two functions) and
  the bridge contract (three message names + the override script) are the shared
  surface; both are small and already spelled once. The suite's fake-window path
  keeps the LOGIC tests platform-neutral.
- **Not in scope here:** the `.app`/codesign/notarize packaging (named as the
  second stage above), and Windows (WebView2, a third backend the same seam
  would take).

## See also

- The shipped Linux backend it parallels: `src-desktop-native/Glance/Desktop/WebKit.hs`
  and the engine-agnostic seam in `src-web/Glance/Desktop/Native.hs`.
- The flag and vendored bindings: `glance.cabal` (`flag native-window`,
  `library glance-desktop-native`) and `cabal.project.native`.
- The setup this extends: [`docs/building.md`](../../building.md) and
  [`flake.nix`](../../../flake.nix) (the Darwin note this would flip).
