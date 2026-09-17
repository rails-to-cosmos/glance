# Proposal — the shell becomes a ports adapter

**Status:** partial · **Date:** 2026-08-19 · **Origin:** user — *"our
front-end could be elm fully, right?  For pure javascript we can use elm's
'require' and organize our app as an Elm application… also uglify+compact?"*

## The finding, in one line

The front end should be an ELM APPLICATION WITH A THIN JS BOUNDARY — widget by
widget, never a rewrite — because the bugs the shell keeps paying for are the
gap between Elm's model and the shell's mirror of it, and each move closes a
piece of that gap by construction.

## The correction the plan rests on

Elm has no `require` and no FFI: since 0.19 the doors are PORTS (async
messages), FLAGS (init data) and CUSTOM ELEMENTS.  Whatever JS the app needs
stays on the far side of a port, so "fully Elm" is not a reachable end state —
"Elm owns the state and the keys, JS owns the platform edges" is.

## Why move — the evidence is this week's bugs

- `docs/bugs/fixed/2026-08-19-a-key-after-a-drawn-step-acts-on-the-row-behind-it.md`:
  the shell's keys act on a MIRROR that lands a macrotask behind the draw.
  Three of four full runs failed, rotating cases. The fix (`data-id`,
  `docAtNow`, settled walks) is a discipline. Moving each operation behind an
  Elm command removes its dependence on that mirror.
- The dirty-baseline bug (fixed in `a74685c`'s follow-up) was the same gap:
  `fill()` read mirrors that had not landed.  THE COMMIT CARRIES ITS OWN CARGO
  and THE BASELINE COMES OFF THE FILL are both compensations for state living
  on two sides.
- The pattern has paid twice already: the pane's parse and splice moved into
  `Doc.elm` and earned 159 unit tests; `Listing.elm` replaced three hand-rolled
  lists ("THE SMALL LISTS ARE ONE ELM PROGRAM").

## What can never move

- **`assets/table-view.js` stays vendored.**  One renderer, two hosts, refreshed
  by `make sync-renderer`; a rewrite is a fork of the sibling contract.  It
  keeps its bridge (the flag-port shape `dmount` already wears, or a custom
  element).
- **The platform edges**: WebSocket (out of Elm core since 0.19),
  `localStorage`, focus and caret in raw fields, `scrollIntoView`.  These are
  the ports adapter — a few hundred lines, down from the glue's ~3,700.

## The sequence, one widget per step, gate green between

1. **The doc pane's keys into Elm.** Elm publishes the keys it accepts with
   each state message. The shell uses that capability only to claim the DOM
   event synchronously, then forwards the unchanged key. Key meaning and the
   resulting state transition live together in Elm. Move the remaining key
   families the same way until the keydown dispatch contains only overlay
   commits and sheet chrome; mirrors retire as their last platform reader does.
2. **The prompt palette as an Elm widget.**  It is a list with a cursor and a
   narrow — `Listing.elm`'s own shape; `askText`'s `raising` flag (the eaten
   first key) is another mirror-gap compensation that dies here.
   `../partial/2026-08-18-generalize-prompt-kind.md` already points this way.
3. **The popups' clamp-and-scroll** joins the same program;
   `Glance.Web.Page.Popups`' registry becomes its flags.
4. **The overlay edits render inside the rows.**  `placeEdit` measures rects to
   float a textarea over the line; an Elm-rendered field IN the row needs no
   measuring, and `sizeDocEdit`'s wrapped-height dance
   (`docs/bugs/2026-08-18-a-continuation…`) goes with it.
5. **The shell that remains** — fetch, WS, storage, boot, table bridge — is the
   adapter, and the glue's string-pin tests retire widget by widget into
   elm-test units plus keyed harness drives (the harness already inits
   `Elm.Doc`; it keeps working).

Each step deletes a compensation the repo currently documents as a law.  The
laws to retire are named in AGENTS.hs: the mirror note, the cargo note, the
baseline note, the mirror-agreement note.

## Minify the bundle — independent of all the above, and ready now

The official recipe over our actual bundle:

|                  | raw    | gzipped |
| ---------------- | ------ | ------- |
| `assets/elm.js`  | 266 KB | 54.7 KB |
| minified         | 62 KB  | 21.5 KB |

−77% raw, −60% on the wire.  `make elm` gains the two-pass step
(`--compress 'pure_funcs=[F2..F9,A2..A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe'`
then `--mangle`).  Two repo facts to honour:

- `assets/elm.js` is a COMMITTED BUILD INPUT the node harness evals; mangling
  is eval-safe but the committed diff becomes opaque, so `make elm` must stay
  byte-reproducible (pin the minifier's version) or the bundle stops being
  reviewable at all.
- TestSpec greps the bundle for STRING LITERALS (`'dbul'`), which survive
  mangling; a pin on a `$author$project$…` symbol name would not.  The sweep
  that forbids symbol-name pins is one line in the spec.

## Sequencing

The minify step is independent and cheap: first.  Step 1 (keys into Elm) is
the highest-value move and the next one after this proposal is accepted; each
later step stands alone behind a green gate.  No step is a rewrite, and the
table's bridge is the boundary that never moves.

## 2026-09-17 reassessment

The new front-end rule is to reuse widgets whose interaction has already been
proved. That changes the destination for ordinary lists. `Listing.elm` now
duplicates the shared renderer's table, cursor, flags, and narrowing model for
only the links and tags popups; settings and refer already demonstrate that
`TableView` can host these shapes. Retire `Listing.elm` after a compact inline
TableView spike instead of extending it. Apply the same test to the prompt
palette before porting that custom widget to Elm.

The durable Elm boundary is narrower: `Doc.elm` owns the material document and
its custom editing state machine. The next Elm move remains the document-local
key family because it deletes a JS mirror of Elm-owned state. TableView surfaces,
page routing, fetch, WebSocket, storage, focus, caret and viewport geometry stay
in a typed browser adapter. This preserves the proposal's state-ownership goal
while following the stronger widget-reuse rule.

## 2026-09-17 implementation — reproducible bundle compaction

The independent bundle step is complete. `make elm` compiles an optimized
temporary program, compresses it with the Elm runtime's pure curry/apply
helpers, then mangles it in a separate pass. Both passes use
`terser@5.44.0`; after the first key-family move the committed result is 77,490
bytes raw and 26,166 bytes gzipped, down from 318,232 and 63,657 bytes
respectively.

The offline suite now checks the pinned two-pass recipe, the bundle size, and
program exports and semantic string markers that survive mangling. It no
longer relies on generated Elm symbol names or a minifier's quote style.

## 2026-09-17 implementation — document navigation keys

The first complete key family now belongs to Elm: row movement, finer and
broader grain movement, their aliases, and `B` to climb. One Elm binding table
both interprets these keys and publishes its key roster in `docState`. The
shell turns that roster into a `Set`, uses membership to call
`preventDefault()` in the browser event, and forwards `{ kind: "key", key }`.
It contains no map from those keys to document commands.

This capability handshake resolves the event-ownership problem without a
second semantic registry. JavaScript owns the synchronous DOM decision; Elm
owns what the claimed key means. Programmatic table walks retain the explicit
`step` message and do not arm a keyboard echo. The remaining document key
families and state mirrors keep this proposal partial.
