# Bug — a full browser run fails about once in a dozen, and the screenshot looks right

**Status:** open · **Reported:** 2026-08-20 · **Browser:** Chromium
· **Surface:** `make browser-check`, full runs only

## Symptom

Roughly one full run in ten fails; every failing case is green run alone and
green on the immediate rerun. Seen tonight on case 34 (the drawer stop), case
11 (the box-over-row edit) and, caught with artifacts, case 36 (the child
splice) — a different case each time, which is the old rotating shape
(`2026-08-19-a-key-after-a-drawn-step-acts-on-the-row-behind-it.fixed.md`).

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

## To catch it properly

```sh
for i in $(seq 20); do
  out=$(make browser-check 2>&1) || { echo "$out" > /tmp/flake.txt; break; }
done
```

The runner prints the failing assertion and an artifacts dir on the red run;
`flake.txt` plus the artifacts' `NN.png` is the missing half of this file.
