# Bug — a key pressed after a drawn step acts on the row the reader just left

**Status:** fixed · **Reported:** 2026-08-19 · **Browser:** Chromium
· **Surface:** `make browser-check`, rotating cases · **Fixed in:**
`frontend/elm/src/Doc.elm`, `frontend/glue/20-sheet.js`, `test/browser/cases.mjs`

## Symptom

Full runs failed at roughly **three in four**, each time on a DIFFERENT case —
1, 2, 10, 13, 17, 18, 19, 20, 27, 32 were all seen — and every failing case was
green run alone. The failing step was always a cursor key (`RET`, `+`, `d`)
pressed right after a wait that watched the DOM: the box never opened, the
draft never drew, the picker never rose. Bad runs stalled 30–39s against a
21s baseline — one or two 8s timeouts.

## Root cause

The pane is an Elm program and the shell keys act on a MIRROR of its state
(`drows`/`dat`), pushed over a port. One `update` schedules BOTH the redraw
(requestAnimationFrame) and the port push (a macrotask, the outgoing-port
manager riding `Process.sleep 0`) — and rAF can fire FIRST. A driver that
waits for the DRAW to show the cursor on its target row can then press a key
while the MIRROR still points at the previous row. `RET` answered for the
drawer instead of the paragraph ("f reaches the rows inside"), `+` raised the
property prompt instead of a draft, and the case timed out waiting for a box
that was never asked for.

The window is one frame — humanly unreachable, mechanically reliable. It
rotated across cases because whichever case's walk lost the race that run
paid; the family predates this week (the old case-32 flake was the same
race with a different face).

## Evidence

- The mirror comment: "THE DOCUMENT PANE IS AN ELM PROGRAM; the MIRROR below
  is a macrotask behind it" (`frontend/glue/20-sheet.js:4`).
- A failing case-13 screenshot: point drawn on the paragraph, grounded, no
  edit box, echo silent — the RET was answered by the drawer.
- A failing case-2 run: `+` on a drawn paragraph produced no `.d-draft`; the
  same run's key prompt machinery answered instead.
- The harness's independent sighting: the node shell found the outgoing-port
  managers boot on `Process.sleep 0` and had to run 0 ms timers inline to make
  the first round trip synchronous.

## The fix

Three parts, one law — **a driver that watched the draw must also see the
mirror agree before a key**:

- Every row div carries `data-id` (`Doc.elm`), so the DOM says which row it
  drew.
- The glue exposes the mirror's cursor as `var docAtNow` — the suite-var
  precedent `whichKeys` already set (`20-sheet.js`).
- `walkTo` returns only when `at.dataset.id === docAtNow()`; a `settled(p)`
  helper imposes the same agreement before every key that follows a hand-rolled
  DOM wait (`cases.mjs`).

Measured: **3 of 4 full runs red before; 0 of 6 after, walls 21.1–21.8s** (bad
runs had stalled to 30–39s). A late-arriving reload could still shut a
just-opened edit — a second, rarer face of the same season — and `reload()`
now re-checks `sheetOpen()` when its fetch returns, the frame-time guard said
again at arrival.
