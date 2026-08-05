# Proposal — the glue becomes a JavaScript file

**Status:** proposed · **Date:** 2026-08-05 · **Origin:** user, pointing at
`Glue.hs:5234` — "the whole file is just JS code, design it better"

## The measurement that decides it

`Glue.hs` is 5242 lines of JavaScript spelled as a Haskell string list. Its
entire reason for being Haskell is NINE splice sites: `DCELLS`, `PLANNING`,
`FOLLOWABLE`, `CODES`, `LCOLS`, `TCOLS`, the three `LOG` numbers — all
build-time constants off `Glance.Query` — and ONE per-request value,
`DEFAULT_QUERY` (the store's served filter). Every other line pays the
string-quoting tax (`\"` everywhere, no editor mode, no linter, no
`tsc --checkJs`, Haskell-shaped diffs of JS changes) for nothing.

## The design

**Code and data separate, each by the repo's own precedent.**

1. **`assets/glue.js`** — the script as a real file, TH-embedded by the same
   `embedFile` splice that already carries `assets/table-view.js`, served by
   the same asset route (content type, gzip, `--assets` override all
   inherited). The binary stays the whole deployment; nothing new is fetched
   from anywhere else.
2. **One config object** replaces all nine splices: the page emits
   `<script>window.GLANCE = {…}</script>` ahead of the two script tags —
   the keymap blob already rides the page this way, so this is that pattern
   finishing the job. Eight members are per-build constants; `defaultQuery`
   is the one per-request member and the reason the blob is emitted by
   `Page`, not baked into the file.
3. **`Glue.hs` shrinks to the blob builder** (~40 lines): the nine values,
   `jsonValue`d into one object. The module keeps its name and its place in
   the dependency order; `Page` reads it as before.

## What it buys

- Real tooling: the tv repo's own `jsconfig` + `tsc --checkJs` discipline
  applies to our 5k lines for the first time; eslint if wanted; an editor
  that knows what language it is in.
- Honest diffs and blame for the file that changes most.
- The suite gets SIMPLER: `glueOf` stops extracting script text from served
  HTML and reads the embedded asset; every needle keeps its spelling. The
  node harness `require`s the file and supplies `window.GLANCE` itself —
  today it stubs those constants anyway.
- The WASM transport adapter (ports proposal, host 4) gets a real file
  boundary to cut at instead of a Haskell list.

## Migration, verbatim-provable

The transform is textual, so the ~1900 Haskell comment lines survive as `//`
comments beside the code they document:

1. Mechanical pass over `Glue.hs`: string lines unquoted and unescaped,
   `--` comment blocks to `//`, the nine splice lines rewritten by hand to
   `window.GLANCE.*` reads.
2. Proof of faithfulness: dump the OLD `shellGlue` output and diff against
   the new file with comments stripped on both sides — byte-equal modulo the
   nine known lines, or the migration does not land.
3. `Page` emits the blob + `<script src>`; `Routes` embeds the asset;
   the harness points at the file; full suite + corpus.

## Doctrine edits (the honest cost)

- CLAUDE.md's "vanilla inline JS" becomes "vanilla JS, compiled in" — the
  no-framework/no-build-step/no-dependency halves all stand; `cabal build`
  was always the build step.
- "One `<script src>`" becomes two, both answered out of the binary.
- `--assets` semantics: the directory already replaces the WHOLE asset set,
  so live glue hacking comes free — edit `assets/glue.js` in place, serve
  with `--assets assets`, no rebuild per tweak. A new capability, worth its
  own line in the README.

## Later, not now

Splitting the file into modules (transport / keys / surfaces / sheet /
popups / boot) and adopting `checkJs` annotations file-wide are follow-ups —
each is cheap once the extraction lands and worthless before it.

## LOC and risk

Net LOC ≈ neutral (the lines move, minus ~5k quote pairs). Risk is
concentrated in the migration's faithfulness, which step 2 makes a proof
rather than a review; the wire, the routes and every behavior test are
unchanged by design.
