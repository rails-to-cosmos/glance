# Proposal — a real browser drives the page, so geometry is measured

**Status:** done — LANDED 2026-08-11 as `make browser-check`, eight cases, one of them
red against a defect it found on its first run (see "What landed") ·
**Date:** 2026-08-11 · **Origin:** user, after a session
where the paragraph editor grew to ten lines and COVERED the nine lines under
it with 1781 tests green — found by looking, fixed in `cb6db85` — and three
more display bugs were caught the same way the same day.

**The toolchain half is no longer a question.** `make browser` installs Chrome
for Testing 151.0.7922.34 through playwright as a pure DOWNLOADER — no import,
no `node_modules`, no lockfile — into `~/.cache/ms-playwright`, and needs NO
ROOT, which is what makes it available on a machine that packages no chromium.
`make browser-path` answers where it landed, empty and silent when it is
absent, so a driver tests for it rather than parsing an error.

The zero-dependency client was proven end to end against it in a spike: node's
global `WebSocket` onto `/json/version`'s `webSocketDebuggerUrl`,
`Target.createTarget` + `attachToTarget` with `flatten`, then
`Runtime.evaluate` returning BOTH halves this proposal exists for — a real
`getBoundingClientRect` (`height: 37` off a 37px element) and a real
`getComputedStyle` (`37px`). So the remaining work is the CASES, not the
transport.

What is still open is below, unchanged: WHICH ENGINE IS THE CONTRACT. Blink is
what installed here; WebKitGTK is what `make native` ships.
## The measurement that decides it

**Nothing in this repo renders anything.** The suite is 1781 Haskell cases
driving the WAI application in process (`TestServe.hs:21`,
`Network.Wai.Test`), plus 53 elm-test cases over the pure scanner, plus
`make check-glue` — tsc over `assets/glue/*.js`. No socket is bound, no layout
is computed, no pixel is painted.

**The node harness is a DOM with no layout, and says so.**
`test/fixtures/shell-harness.js:1285`: "Geometry is beyond this harness —
nothing here has a layout"; `:1289` returns zeros from every
`getBoundingClientRect`. It has no HTML parser (`:1150`), so the renderer is a
stub object (`:1033`) and the table is never mounted. 331 cases route through
it (`TestServe.hs:5751`).

**So every geometry rule is asserted as CSS SOURCE TEXT.** `groundSweep`
(`TestServe.hs:6248`) greps rule bodies for `border` / `outline` /
`text-decoration` / `box-shadow`. The edit box's floor is asserted as the
STRING `"min-height:calc(var(--g-doc-rows, 1)"` (`TestServe.hs:5893`). Those
sweeps assert that a DECLARATION is present. Where the declaration lands on
screen is unasked.

**Four bugs this session, all in the gap, none caught by a test:**

| bug | fix | what shipped green |
|---|---|---|
| the edit box grew and stood OVER the document | `cb6db85` | `.de.dat`'s floor declared at `Style.hs:146` |
| the pane's flag drew in `--g-warn`, the table's in `--g-bad` | `14e13d9` | both namespaces agreed in the served text |
| an empty drawn paragraph collapsed to zero height | `d7ba44b` | `.d-draft` declared at `Style.hs:165` |
| the state badge lost its colour in the Elm port | `80c3732` | "invisible to 1737 tests", per its own message |

`d7ba44b`'s own comment is the shape of the whole problem: "`:empty` cannot
find it — Elm emits an empty text node — so the height is declared rather than
tested for" (`Style.hs:163-165`). A declaration nothing measures is a
declaration nothing keeps.

The sibling renderer is worse off: `../table-view` has `test` (elisp ERT),
`web-check` (tsc) and `web-perf`, and NO JS harness at all — its palette,
autocomplete and virtualization are unasserted from either side of the wire.

## The toolchain question, decided against this machine

Measured today, on this machine:

- node **v26.7.0** with a GLOBAL `WebSocket` (`typeof WebSocket === "function"`).
- **No Blink anywhere**: `ls /usr/bin | grep -iE "chrom|brave|vivaldi"` is
  empty, no flatpaks, no `~/.cache/ms-playwright`, no `~/.cache/puppeteer`.
- The only browser is **Firefox 153.0.3**. THREE SPIKES to make its Remote
  Agent listen on `--remote-debugging-port` — with and without
  `--remote-allow-hosts` / `--remote-allow-origins` — all failed; the port
  never listened.
- No `geckodriver`, no `chromedriver`. WebKitGTK **2.52.5** is installed (the
  engine `make native` serves the page in), and Arch's `webkit2gtk-4.1` ships
  no `WebKitWebDriver` — `pacman -Ql` lists `MiniBrowser` and the three
  process helpers, no driver binary, and no package in `extra` carries one.
- `npx` 12.0.2 works and the registry answers: `npm view playwright version`
  → **1.62.1**.

Against that:

**(a) Raw CDP/BiDi over the global WebSocket, driving an installed browser.**
Zero dependencies, zero imports, exactly the ephemeral doctrine. The installed
browser is Firefox, whose remote agent did not listen after three attempts, and
whose protocol is WebDriver BiDi — a second protocol client for the engine
`glance desktop` reaches LAST (browser order: chromium/chrome/brave/vivaldi,
then `xdg-open`). Unproven here, and the unproven part is the launch.

**(b) `npx --yes playwright` with a cached Chromium.** ~150 MB into
`~/.cache/ms-playwright`, outside the repo, which the doctrine already accepts
(`~/.elm` is 7.1 MB today). The API is mature: auto-waiting, retrying
expectations, three engines including a WebKit build. The cost is a real one
and it is not the download: **`npx --yes` gives no module resolution.** `tsc`
and `elm` take FILES as input and import nothing from this tree; a playwright
spec `import`s `@playwright/test` from a repo with no `node_modules`. Making
that resolve means `NODE_PATH` surgery or a lockfile — the two things the
Makefile's shape exists to avoid.

**(c) puppeteer-core against a system Chrome.** There is no Chrome. Dead here,
and alive only after someone installs one by hand — at which point it is (a).

### Recommendation: (a), with (b) demoted to a downloader

**`npx --yes playwright@1.62.1 install chromium` puts a browser binary in
`~/.cache/ms-playwright`. Nothing imports playwright. The driver is ~200 lines
of CDP over node's global `WebSocket`.**

This is what each option was actually offering:

- (a)'s failure was FIREFOX'S REMOTE AGENT, and Chromium's
  `--remote-debugging-port` is the best-trodden path in the field. The spike
  failed on the browser, and the fix is a different browser.
- (b)'s value is the DOWNLOAD — a known-good, version-pinned binary on a
  machine with no Blink. Its API is what costs a resolution story. Take the
  binary, leave the API.
- (c) merges in for free: the driver takes `$CHROME` when one is on PATH, so a
  machine with a system Chromium skips the download and drives it.

The CDP surface the assertions need is six domains and stays small:
`Target.createTarget` / `attachToTarget`, `Page.enable` / `navigate` /
`loadEventFired` / `captureScreenshot`, `Runtime.evaluate` (every assertion is
"evaluate JS in the page, return JSON"), `Input.dispatchKeyEvent`,
`Emulation.setDeviceMetricsOverride`. What playwright's API would have bought
above that is auto-waiting, and this repo already knows the rule that replaces
it: **settle on the CONDITION with a cap, never on a duration** (CLAUDE.md's
`until:stale=off`, landed in `6ae720c`).

The driver talks to the page through six calls — `goto`, `eval`, `until`,
`press`, `size`, `shot` — so swapping the CDP client for playwright or BiDi
later is one adapter behind six names. Same rule as the transport surface in
`docs/proposals/2026-08-05-native-ports.draft.md`.

## The design

**Two files, one Make target, no repo dependencies.**

- `test/browser/drive.mjs` — the whole driver: browser launch, CDP client,
  daemon lifecycle, the six-call page handle, the report. ONE WIDGET, ONE FILE.
- `test/browser/cases.mjs` — the assertions, each naming the bug it exists for.
- `test/browser/tree/` — a committed org fixture, three files, each top entry
  carrying its own `ORG_GLANCE_ID` so the driver addresses rows by a STABLE id
  and never computes a `FILE#K` ordinal.

**The daemon.** `make browser-check` builds and resolves the binary with
`cabal list-bin exe:glance` — `run-wasm`'s own shape in the Makefile — and hands
it to node as `$GLANCE_BIN`, so the driver's readiness poll never waits on a
compile. The driver COPIES `test/browser/tree/` to `mktemp -d` and serves the
copy: the cases commit paragraph edits and set states, and the repo's fixtures
stay byte-identical.

```js
const port = await freePort();                       // bind 0, read it, close
const tree = await copyTree("test/browser/tree");    // a temp copy; writes land here
const glance = spawn(process.env.GLANCE_BIN,
                     ["serve", "--dir", tree, "--port", String(port)],
                     { stdio: ["ignore", "inherit", "inherit"] });
// THE BIND IS NOT THE LOAD.  `/' and the assets serve while the walk runs and
// `/headlines' answers 503 + Retry-After until it lands (Web.hs:78-80: the
// walk is forked, Warp binds after it), so readiness is the route that NEEDS
// the store.
await until200(`http://127.0.0.1:${port}/headlines?limit=1`, 20_000);
```

Teardown is one `finally`: `SIGTERM` the daemon, wait for exit with a 5 s
`SIGKILL` fallback, close the CDP socket, kill the browser, `rm -rf` the temp
tree and the browser profile. A run leaves nothing behind, and a run that
crashes leaves at most one temp directory named in the failure report.

**Surfaces open by URL, never by a key sequence.** The page declares
`?page=NAME` for every surface, `&row=ID` for a rowed one and the fragment for
a panel (`SURFACES` in `assets/glue/70-shell.js:17-32`; `bootPage` at `:52`).
So the driver navigates to `?page=sheet&row=ID` and `?page=config#theme` rather
than keeping a second copy of the keymap. Presses are for the gestures under
test.

**Keys carry both halves.** A LETTER BINDING NAMES A PHYSICAL KEY, so
`Input.dispatchKeyEvent` sends `code: "KeyD"` beside `key: "d"`, and an
uppercase binding is `code: "KeyD"` + `key: "D"` + shift, which is what
`keyName` reads. Modifiers are spelled the repo's way: `press("S-Enter")`,
`press("C-x")`.

**The report.** One line per case, failures last, exit 1 on any failure:

```
ok   1 — an open edit moves the line under it down, never covers it
not ok 2 — a paragraph drawn before it is written still owns a line
     the empty paragraph collapsed to 0px, under one line of 18px
     screenshot: /tmp/glance-drive-Xk2p/2.png
     the page's own account: 12:04:31 info  boot   1 row
                             12:04:31 info  sync   ws open
```

The last two lines are the page's LOG STRIP read out of `#log` — the shell's
own append-only account of what it did (CLAUDE.md: scope, severity, one line
per row). A failing geometry case gets the picture and the page's testimony,
which is what a reader would have gone looking for by hand.

**Out of `cabal test`, behind its own target.** The standing precedent is in
the Makefile at `elm-test`: "OUT of `cabal test' on purpose: elm-test fetches
`elm-explorations/test' at run time, and the Haskell suite must stay offline."
This target needs a 150 MB browser binary, a temp tree, a spawned daemon and
fonts. Same rule, harder. A machine with no `npx` and no cached browser says so
and changes nothing — the shape `elm-test` uses and `bootedPage` uses
(`TestServe.hs:5751`, the stderr `SKIPPED` line), because a check that passes
having asserted nothing is the failure mode this repo already names.

```make
# THE ONE CHECK THAT MEASURES A PIXEL, and it is OUT of `cabal test' for
# elm-test's reason one size up: it drives a browser, spawns a daemon and needs
# fonts.  The suite stays offline; this target says what it needs and skips.
browser-check:
	@command -v node >/dev/null 2>&1 || { echo "browser-check: no node on PATH -- SKIPPED"; exit 0; }
	@bin="$$($(MAKE) -s browser-path)"; \
	if [ -z "$$bin" ]; then echo "browser-check: no browser ... -- SKIPPED"; exit 0; fi; \
	cabal build -v0 exe:glance && \
	CHROME="$$bin" GLANCE_BIN="$$(cabal list-bin -v0 exe:glance)" node test/browser/drive.mjs
```

The install stays `make browser`'s: this target never downloads, so a run with
no browser skips instead of pulling 150 MB into a check.

## The first assertions

Every case below is a COMPUTED reading the text suite provably cannot take,
and every one names the bug it exists for. Figures are RELATIONAL — does B
start below A's bottom, is this colour that colour — so no case depends on a
font's advance width.

```js
// test/browser/cases.mjs
export default [

// cb6db85.  The box grew and stood over the document: ten typed lines covered
// the nine under them.  TestServe.hs:5893 asserts the STRING
// "min-height:calc(var(--g-doc-rows, 1)"; where the next line ENDS UP is
// unaskable there, the harness returning zeros from every rect
// (shell-harness.js:1289).
{ name: "an open edit moves the line under it down, never covers it",
  async run(p, base, row) {
    await p.goto(`${base}/?page=sheet&row=${row}`);
    await p.until(() => document.querySelectorAll("#mdoc .de").length > 2);
    await p.press("n");                                  // onto the paragraph
    await p.press("Enter");                              // open it
    await p.until(() => !!document.querySelector("#dpara textarea"));
    for (let i = 0; i < 10; i++) { await p.type(`line ${i}`); await p.press("S-Enter"); }
    const seen = await p.eval(() => {
      const at = document.querySelector("#mdoc .de.dat");
      const under = at.nextElementSibling;
      const a = at.getBoundingClientRect(), b = under.getBoundingClientRect();
      return { grew: a.height, ends: a.bottom, starts: b.top,
               under: under.textContent.slice(0, 24) };
    });
    assert(seen.grew > 100, `the block never grew: ${seen.grew}px for ten lines`);
    assert(seen.starts >= seen.ends - 1,
      `"${seen.under}" is covered: it starts at ${seen.starts}, the box ends at ${seen.ends}`);
  } },

// d7ba44b, Style.hs:163-165.  A paragraph drawn before it is written holds
// nothing and `:empty' cannot find it — Elm emits an empty text node — so the
// floor is DECLARED.  Nothing measures the declaration.
{ name: "a paragraph drawn before it is written still owns a line",
  async run(p, base, row) {
    await p.goto(`${base}/?page=sheet&row=${row}`);
    await p.until(() => !!document.querySelector("#mdoc .de"));
    await p.press("+");
    await p.until(() => !!document.querySelector("#mdoc .d-draft"));
    const seen = await p.eval(() => {
      const cs = getComputedStyle(document.querySelector("#mdoc"));
      const line = parseFloat(cs.getPropertyValue("--g-doc-fs"))
                 * parseFloat(cs.getPropertyValue("--g-doc-lh"));
      return { h: document.querySelector("#mdoc .d-draft").getBoundingClientRect().height, line };
    });
    assert(seen.h >= seen.line - 0.5,
      `the empty paragraph collapsed to ${seen.h}px, under one line of ${seen.line}px`);
  } },

// 14e13d9.  The pane drew a flag in `--g-warn' at a strength of its own, so
// `d' over the table and `d' over the pane — one gesture over one queue —
// looked like two states.  `paletteSweep' (TestServe.hs:6102) compares the two
// NAMESPACES in the served text; it cannot mount the renderer, whose palette is
// injected into <head> AT MOUNT TIME at zero specificity, so what a flagged row
// PAINTS is unaskable there (shell-harness.js:1033 — TableView is a stub).
{ name: "a flag in the document pane paints the table's own red",
  async run(p, base, row) {
    await p.goto(`${base}/?page=sheet&row=${row}`);
    await p.until(() => !!document.querySelector("#mdoc .de"));
    await p.press("d");
    await p.until(() => !!document.querySelector("#mdoc .de.dfl"));
    const seen = await p.eval(() => {
      // Resolve both through the engine: a hex token and a computed shadow
      // string are the same colour only once something has painted them.
      const rgb = (v) => { const d = document.createElement("div");
        d.style.color = v; document.body.append(d);
        const c = getComputedStyle(d).color; d.remove(); return c; };
      const root = getComputedStyle(document.documentElement);
      return { edge: getComputedStyle(document.querySelector("#mdoc .de.dfl")).boxShadow,
               table: rgb(root.getPropertyValue("--tv-flag").trim()),
               page: rgb(root.getPropertyValue("--g-bad").trim()) };
    });
    assert(seen.page === seen.table,
      `--g-bad paints ${seen.page} and --tv-flag paints ${seen.table}`);
    assert(seen.edge.includes(seen.table) && seen.edge.includes("inset"),
      `the pane's flag edge is "${seen.edge}", the table's red is ${seen.table}`);
  } },

// CLAUDE.md: "The page never scrolls: body is 100vh, overflow:hidden"
// (Style.hs:58).  Every surface opens by its own URL, which is the page's own
// contract (SURFACES / bootPage, 70-shell.js:17,52), so this sweep keeps no
// copy of the keymap.  The KEY LINE scrolls sideways INSIDE itself by design
// and is exempt: the reading is the DOCUMENT's scroller.
{ name: "the page never scrolls sideways, at any width or surface",
  async run(p, base, row) {
    for (const [w, h] of [[360, 720], [800, 900], [1400, 900]]) {
      await p.size(w, h);
      for (const page of ["", "sheet", "config", "tags", "links"]) {
        await p.goto(`${base}/?${page ? `page=${page}&row=${row}` : ""}`);
        await p.until(() => !!document.querySelector("#app"));
        const seen = await p.eval(() => {
          const e = document.scrollingElement;
          const past = [...document.querySelectorAll("body *")]
            .filter((n) => !n.closest("#keys") && n.getBoundingClientRect().right > innerWidth + 1)
            .map((n) => n.id || n.className).slice(0, 5);
          return { over: e.scrollWidth - e.clientWidth, past };
        });
        assert(seen.over <= 1,
          `"${page || "table"}" at ${w}px scrolls ${seen.over}px sideways; past the edge: ${seen.past}`);
      }
    }
  } },

// 80c3732: "THE STATE BADGE LOST ITS COLOUR ... the Elm view invented a CSS
// variable name" — invisible to 1737 tests.  The hue is handed over WITH the
// cell now (20-sheet.js:14, `badgeColor(val, k)'), worn as an inline `color'
// on `span.dc.dc-state' (Doc.elm:838-848).  The table's own pill is
// `.tv-badge', drawn by a renderer no text case mounts.  ONE KEYWORD, TWO
// SURFACES, ONE PAINTED COLOUR.
{ name: "a badge in the sheet paints the hue its column paints in the table",
  async run(p, base, row) {
    await p.goto(`${base}/`);
    await p.until(() => !!document.querySelector("#app .tv-badge"));
    const table = await p.eval(() => {
      const pill = document.querySelector("#app .tv-badge");
      return { word: pill.textContent.trim(), colour: getComputedStyle(pill).color };
    });
    await p.goto(`${base}/?page=sheet&row=${row}`);
    await p.until(() => !!document.querySelector("#mdoc .dc-state"));
    const sheet = await p.eval((word) => {
      const cell = [...document.querySelectorAll("#mdoc .dc-state")]
        .find((n) => n.textContent.trim() === word);
      return cell && getComputedStyle(cell).color;
    }, table.word);
    assert(sheet === table.colour,
      `${table.word} paints ${table.colour} in the table and ${sheet} in the sheet`);
  } },
];
```

Wave two, once the driver is standing and each costing ~10 lines: a popup
clamps inside the viewport (the `--g-pop-max` chain, `Style.hs:54`, nothing
measures it); the tags flush to the far edge on a headline with no title
(`margin-left:auto`, CLAUDE.md's `org-tags-column` rule); the cursor's
scroll-margin band is three of the pane's own lines; `@media (pointer:coarse)`
under an emulated touch viewport.

## What landed

`make browser-check`, eight cases, **6.9 s** of driver on a warm browser
(14.4 s including `cabal build -v0 exe:glance`). The design above stands; three
things it did not say are now in the driver.

**THE FIRST RUN FOUND A DEFECT, and it is the popup-clamp case's.** `.pop-sheet`
declares `height:var(--g-pop-max)` and NOTHING gives the box
`box-sizing:border-box` — the reset spells it for `body` and `#app,#log` alone
— so `#sheet` draws its own 14px padding and 1px border OUTSIDE the cap and
stands **30px taller than it was told to**: at a 480px-tall viewport it runs
`24px..486px` against a cap that computes to `432px`. `5vh + 90vh + 30px >
100vh` wherever the viewport is under **600px** tall, which puts the sheet's
foot off screen on a split window and on any phone in landscape. The fix is one
line in `Glance.Web.Page.Style` — `.pop-band,.pop-sheet{box-sizing:border-box}`
— and is outside the run that found it.

**So a case may be `known`, and a `known` case is NOT AN XFAIL.** It carries the
sentence naming the defect, it prints its reading like any other case, and a
`known` case that GOES GREEN is counted as a FAILURE — "the known defect is
gone, take `known` off". So the field retires itself the day somebody fixes the
rule, and it cannot be used to silence a case that merely broke.

**And no case ships unfalsified: `BREAK=name` takes ONE rule out of the served
page** with a stylesheet injected at document start, each entry naming the case
it must turn red. `make browser-check BREAK=edit-covers` is the proof, and eight
breaks cover the eight cases. Nothing in `src*/` or `assets/` moves to see a
case fail, which is what makes the proof cheap enough to keep running.

The readings, on this machine, at 1400×900 in the default dark theme:

| # | case | what it measured |
|---|---|---|
| 1 | an open edit moves the line under it down | box `23.5px → 199px` for ten lines, the line under it from `184.3px` to `359.8px` |
| 2 | a drawn paragraph still owns a line | the drawn row `23.5px`, `.d-draft` alone `22.8px`, one line `20.8px` |
| 3 | one flag red on both surfaces | `--g-bad` and `--tv-flag` both `rgb(231, 76, 60)`; the pane's edge `rgb(231, 76, 60) 3px 0px 0px 0px inset` |
| 4 | the page never scrolls | `0/0` sideways/down over 5 surfaces × 3 widths (360, 800, 1400) |
| 5 | a popup clamps inside the viewport | **KNOWN RED**: `#sheet` runs `24px..486px` of a `480px` viewport |
| 6 | one keyword, two surfaces, one hue | `TODO` paints `rgb(224, 175, 104)` in the table and in the sheet, against the page's own `rgb(255, 255, 255)` |
| 7 | a paragraph sits under the title text | stars `151px`, title `211.8px`, paragraph box `151px`, its text `172.6px` |
| 8 | the cursor is a ground, and only where the keys are | `rgb(55, 61, 79)` against `rgba(0, 0, 0, 0)`; `rgba(0, 0, 0, 0)` once `TAB` takes the keys |

Cases 6, 7 and 8 are wave two brought forward: each cost ~15 lines because the
driver was already standing, which is the marginal cost the estimate claimed.

Two departures from the sketch, both about honesty of the reading:

- **Case 2 takes TWO readings.** `+` leaves the draft AT POINT and `.de.dat`
  carries a floor of its own while an edit is open, so the real row alone cannot
  say which rule held it up. A PROBE wearing `.de.d-para.d-draft` without `.dat`
  is appended beside it and measured, which attributes the height to
  `.d-draft` itself.
- **Case 4 waits for each surface BY ITS OWN CONTAINER** (`#modal`, `#config`,
  `#tags`, `#links`). A surface that never rose would have the sweep measure the
  table three times over and report `ok`.

## Files

`test/browser/drive.mjs` (new), `test/browser/cases.mjs` (new),
`test/browser/tree/*.org` (new, three files), `Makefile` (one target),
`CHANGELOG.md` (one line). STILL OWED: a `CLAUDE.md` Build bullet naming the
target and its skip rule, and `docs/invariants.md` moving the rules the eight
cases now hold off **none**.

Nothing in `src*/`, `app/` or `assets/` changes. The shipped binary is
byte-identical.

## LOC estimate

As landed: **+404 driver, +362 cases (eight), +55 fixture org, +26 Makefile**
= 847 new lines, 0 changed outside the Makefile. Marginal cost of case N+1
stays 10–20 lines and no new machinery — every case is `goto`, `until`,
`press`, `type`, `eval`, `assert` — plus one `BREAKS` entry, which is the line
that keeps it falsifiable.

## What it will NOT catch

- **Everything above the page.** The parser, the spans, the write path and the
  wire stay the Haskell suite's, and nothing here duplicates them.
- **The engine it does not run.** `make native` ships WebKitGTK 2.52.5; this
  drives Blink. A layout bug that lives only in WebKit ships green. Driving the
  shipped engine needs `WebKitWebDriver`, which Arch does not package
  (`pacman -Ql webkit2gtk-4.1` carries `MiniBrowser` and three process helpers,
  no driver) — a build, not an install.
- **Fonts.** Headless Chromium picks its own defaults out of the machine's 1159
  faces. Any case asserting an absolute pixel figure is a case that will drift;
  the five above are relational for exactly that reason, and an absolute figure
  in a future case is a bug in the case.
- **What a reader would call ugly.** Overlap, collapse and overflow are
  decidable; balance, rhythm and hierarchy are not. This closes the class the
  four bugs came from and opens no other.
- **A schedule no `until` names.** Every wait is a condition with a cap, so a
  rule that breaks only under timing nothing polls for stays invisible.
- **The renderer's own internals.** `../table-view` still has no JS harness;
  this drives the renderer as the page mounts it and asserts what the PAGE
  contracts on. Its palette and virtualization stay unasserted from both sides.

## What it costs to keep green

- **The download**: ~150 MB into `~/.cache/ms-playwright`, once per machine and
  once per pinned version bump. Outside the repo, the standing rule.
- **CI without a browser**: the target is out of `cabal test`, so a run with no
  `npx` prints one line and exits 0. `cabal test` stays offline and stays the
  contract.
- **Flake budget**: one daemon and one browser per run, both torn down in a
  `finally`. Every wait is `until(condition, cap)` — a duration wait in a case
  is the flake, and this repo already paid for that lesson (`6ae720c`).
- **Selector rot**: the cases read `.de`, `.de.dat`, `.d-draft`, `.dc-state`,
  `.tv-badge`, `#mdoc`, `#log` — markup CLAUDE.md already contracts on. A
  rename breaks the driver loudly, which is the correct failure.
- **Wall time**: the fixture tree is three files, so the walk is instant and a
  run is browser launch plus five cases — seconds, and it will grow with the
  case list.

## Existing precedent

- `make elm-test`: a check OUT of `cabal test` because it fetches at run time,
  with the reason written at the target (Makefile).
- `bootedPage` (`TestServe.hs:5745-5765`): the suite already spawns `node` with
  a script and arguments, reads JSON back, and prints `SKIPPED - node is not on
  PATH` to stderr rather than passing silently.
- `make check-glue`, `make elm`: ephemeral `npx --yes`, no `package.json`, no
  `node_modules`, no lockfile — and a pinned version (`elm.json` must say
  `0.19.2`; `0.19.1` is a hard refusal).
- DERIVED ORACLES: `paletteSweep` (`TestServe.hs:6102`) reads the SERVED page
  and compares the two namespaces role by role; `groundSweep`
  (`TestServe.hs:6248`) asserts what it swept FIRST so an empty sweep cannot
  pass. Both cases here that read a colour are that idea one layer down — the
  ENGINE resolves both sides.
- `docs/invariants.md` carries 40 entries whose evidence is **none**
  (`docs/invariants.md:6` defines the legend). Four of them are what this
  proposal moves; the unbounded `hubPending` (`Store.hs:293`, forked at
  `Web.hs:78`, no cap, no restart) is not one and stays where it is.

## Open decisions

1. **WHICH ENGINE IS THE CONTRACT.** Wave one drives Blink because Blink is
   what a downloader can put on this machine. `make native` ships WebKitGTK,
   and `glance desktop` on a machine with no Chromium falls through to
   `xdg-open` — Firefox here. Making the driven engine the SHIPPED engine costs
   building `WebKitWebDriver`, and the answer changes if the native window
   becomes the primary host. **A human takes this one.**
2. Whether `make browser-check` gates a commit or is a sweep run by hand. Wave
   one is by hand; a gate needs the flake budget spent first. What a gate would
   also need is the `known` case FIXED — a gate carrying a permanent expected
   red is a gate nobody reads.
3. Whether the driver ever grows screenshot BASELINES. The recommendation is
   no: a baseline is a second artifact to keep true and headless font rendering
   moves it. Screenshots stay a FAILURE ARTIFACT.
4. Whether the fixture tree carries an `.org-glance` store, which would put the
   blob/capture/delete surfaces under the driver. Wave one serves a plain tree.
