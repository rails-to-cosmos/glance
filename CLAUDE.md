# glance — invariants

Rules the code silently enforces. Violating one is a bug even when the suite
stays green. Fuller version with evidence: [docs/invariants.md](docs/invariants.md).

## Spans

- Spans are half-open CHAR offsets `[start, end)` into the `orgParse` input
  `Text` — never bytes, never line/column.
- Headline sub-spans are tight: each slices to exactly its component, no
  surrounding whitespace. Element spans are only well-formed + reparseable.
- `hsFull` runs from the stars to the max end of present components and never
  covers trailing whitespace. Sub-spans nest inside it, ordered
  todo < priority < title < tags < planning < properties, non-overlapping; a
  drawer, when present, ends exactly at `hsFull`'s end.
- The three planning spans permute freely on their line, so `headlineSpanParts`
  and the `hsFull` fold sort them by `spanStart`. Each covers the timestamp
  text alone — the keyword is not part of it.
- `stripSpans` must cover every span-carrying constructor; a new `Element`
  constructor that embeds spans must extend it.
- A subtree span runs from a headline's stars to the next headline at its level
  or shallower, else to the end of the document; they nest, non-nesting pairs
  are disjoint, and trailing blank lines belong to the subtree above.

## Parser

- A top-level element must end at whitespace or EOF; a sub-parser stopping
  mid-word fails the WHOLE file (the known 13-file corpus failure class).
- Headlines parse only at column 1, via the threaded begin-of-line Bool.
  Never `getSourcePos` — quadratic on failure-heavy input.
- TODO keywords are matched case-sensitively and stored verbatim;
  pragma/property KEYS are uppercased.
- In `spannedContainerUntil` the end-parser branch precedes the hspace-eol
  branch (tags open with `hspace1` and lose it otherwise).
- Trailing hspace terminates a container and stays unconsumed.
- The property parser rejects reserved `PROPERTIES`/`END` — that guard is what
  terminates the drawer.
- Timestamp range halves share one bracket kind; `tsmHasTime` alone decides
  whether a time renders; the weekday is recomputed from the date.
- A range is spelled `<a>--<b>` or compactly as `<date wd 10:30-11:30>`;
  `tsCompactRange` preserves which, and the renderer never canonicalizes one
  into the other (CLOCK lines are always `--`). A `-` before a time opens a
  range end, before a unit it is a negative repeater.
- The planning line is the one line after the title line, before any drawer:
  `SCHEDULED:`/`DEADLINE:`/`CLOSED:` uppercase-only, any order, last-wins per
  keyword. `CLOCK:` is not one. The whole line backtracks when it is not a
  planning line, and a `SCHEDULED:` further down the body stays body elements.
- `orgParse` on error returns zero elements AND the caller's context untouched.
- Context keyword sets are append-only; a `#+TODO:` affects only headlines
  below it; no Context merge operation exists — `defaultContext` seeds
  TODO/DONE.

## Render

- `TextShow` is a lossy REPL re-serializer (whitespace collapse, uppercased
  pragma keys, Set-ordered keyword lists). Never use it for write-back or the
  wire contract; spans are the only lossless channel. TestRoundtrip's
  exact-vs-stable split IS the documented lossiness budget.

## Scan

- Every accumulator is forced at each step; `forceResult` runs inside
  `evaluate` + `try`. Budget: ~19 MB max residency over 6305 files. `Cursor`
  assumes non-decreasing span starts.
- Corpus check: `cabal run -v0 glance -- scan ~/sync` — expect 0 span
  violations, ~13.3k headlines, wall ~14 s warm.

## Architecture (docs/proposal-org-console-web.md, docs/plan-org-console-web.md)

- Org files are the single source of truth; no second authoritative store.
- Write-back (S8) = surgical span replacement, optimistic lock, atomic
  temp+rename; untouched bytes stay byte-identical.
- Write-back engine = `Data.Org.Edit`: char-span splice, drift-checked, atomic
  same-dir rename, content-agnostic (no `TextShow`).
- `Display`/`TextShow` stay out of the wire contract; the web layer is the
  private sublibrary `glance-web` (`src-web/`, `Glance.Web*`) with the public
  library alone in its `build-depends`, and it binds 127.0.0.1 until S7 brings
  privilege tiers.
- The served store is an in-memory projection keyed by path, so `Map.elems` is
  walk order and `/headlines` equals a fresh `loadDir`. The watch re-parses one
  file per event from `defaultContext`; a failed load keeps that file's rows and
  streams nothing.
- The socket carries SCHEMA.md's row ops alone. A column change (the TODO
  keyword union moving) closes it with reason `view-changed` and the client
  re-fetches. The bootstrap `set-rows` is snapshotted inside the subscribing
  transaction, so there is no journal and no gap. A client whose bounded mailbox
  fills is dropped — the watcher never waits on a browser.
- The public library exposes `Glance.Query` alone over the private
  `glance-internal` sublibrary; cells are sliced from spans and the view
  `Value` is hand-built — no `ToJSON` on an internal type
  (table-view/SCHEMA.md is the contract).
- Materialize: `GET`/`POST /headline?id=…` serves and replaces a headline's raw
  subtree. The digest is pinned at load, any divergence is a 409 with the file
  untouched, and the write path never touches the store — the file watch is the
  only thing that updates rows.
- The served pages fetch nothing off this server: inline styles, inline glue,
  and one `<script src>` naming a file under `--assets`. No CDN, no web font, no
  analytics. The JetBrains Mono `@font-face` appears only when the assets
  directory holds the file, pointing at a bare name this server serves.
- The shell's keymap is `Glance.Web`'s `sharedKeys` + `keyProfiles` and nothing
  else: the page carries them as a JSON blob and its own dispatch parses that
  blob. Movement is the only thing a profile changes (`emacs` default, `vim`);
  the effective map is `shared ++ profile`, and within one no sequence is bound
  twice or opens a longer one. Sequences and command names are org-glance's; a
  row with no handler is recognized and says what will back it. `C-c`/`C-x` are
  prefixes only with the selection collapsed; `C-l`, `C-r`, `C-t`, `C-w`, `C-n`,
  `C-p` and `<f5>` are never claimed, so no profile moves on `C-n`/`C-p`.
- Browser writes are commands over the bridge: structured ones (toggle, retag,
  reschedule) and drift-locked raw replacement (materialize a subtree, later a
  file). Semantic org editing — refile, agenda logic — stays out of the browser.
  Automation = reviewed deterministic scripts, no LLM in the loop.

## Build

- `glance.cabal` is hand-maintained; package.yaml/hpack removed — do not
  regenerate.
- Components: private sublibrary `glance-internal` (`src/`), public library
  `glance` (`src-query/`, `Glance.Query` only), private sublibrary
  `glance-web` (`src-web/`) on the public library alone, one CLI dispatching
  to both sublibraries, one suite naming all three. A new web or daemon target
  depends on the public library alone.
