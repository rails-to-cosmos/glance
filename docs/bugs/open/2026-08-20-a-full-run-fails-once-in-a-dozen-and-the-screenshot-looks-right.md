# Bug — a full browser run fails about once in a dozen, and the screenshot looks right

**Status:** open · **Reported:** 2026-08-20 · **Browser:** Chromium
· **Surface:** `make browser-check`, full runs only

## Symptom

Roughly one full run in ten fails; every failing case is green run alone and
green on the immediate rerun. Seen tonight on case 34 (the drawer stop), case
11 (the box-over-row edit) and, caught with artifacts, case 36 (the child
splice) — a different case each time, which is the old rotating shape
(`../fixed/2026-08-19-a-key-after-a-drawn-step-acts-on-the-row-behind-it.md`).

## Evidence

- A caught run: `not ok 36`, wall 22.0s (no stall), and the failure screenshot
  shows a CORRECT end state — the edit synced to the file, point on the
  child's paragraph, the crumb naming the child. Whatever assertion fired, the
  pane got there; the read raced the render or arrived one turn early.
- Ten consecutive full runs directly after: all green. The failing assertion
  line was not captured (the first capture filtered it out; the second never
  fired).

## Suspected family

The mirror-race season's third face: `walkTo`/`settled` hold keys until the
DOM and `docAtNow()` agree, but a bare `p.eval` that READS (classes, computed
styles, textContent) right after `settled` has no such gate against a
late-arriving `reload()` re-render swapping the rows mid-read.

## Caught faces (2026-08-24)

- Case 38, three times in ~30 full runs, green alone and on every rerun.
  The assertion each time: `the key offers [{"word":"OW","hint":"new"}]` —
  the pair box's key offers read before the sheet's `/properties` answer
  landed (`askVocab` repaints on arrival, `20-sheet.js:509`; the case read
  between the ask and the answer). Same family: a read racing an async
  arrival that has its own repaint.
- Case 44 had a SEPARATE deterministic-ish race — the widget's rect
  measured before the box's placement landed (`#ddate` with no inline
  style while the pane was still drawing the fresh doc). Fixed in the case
  itself: `widgetUp` now waits for the box's own inline `top`
  (`cases.mjs`), so that face is closed, not this file's.
- Case 39, the pair case, caught twice in ~20 runs — once in a full run and
  once running ALONE, which is what made it catchable. The failure
  screenshot shows the offer list holding **both** `OW (new)` and `OWNER`,
  so the read arrived one repaint early: `p.until` returned the moment
  `#doffer` wore `on`, which it does on the typed literal ALONE, and
  `/properties` landed with its own repaint a beat later. Same face as case
  38's. **Closed in the case**: the wait is now for a SECOND entry rather
  than for the class (`cases.mjs`, the `keyOffers` read) — the 8s cap still
  reds the case where the vocabulary never comes. 20 alone-runs green after,
  against 1 red in ~15 before.

## To catch it properly

```sh
for i in $(seq 20); do
  out=$(make browser-check 2>&1) || { echo "$out" > /tmp/flake.txt; break; }
done
```

The runner prints the failing assertion and an artifacts dir on the red run;
`flake.txt` plus the artifacts' `NN.png` is the missing half of this file.
