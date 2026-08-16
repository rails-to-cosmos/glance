# Bug — the composite case sometimes finds a pane with no leaves in it

**Status:** open · **Reported:** 2026-08-17 · **Browser:** Chromium
· **Surface:** `make browser-check`, case 9

## Symptom

`make browser-check` fails roughly **one full run in eight**, always on the
same case and always with the same words:

```
not ok 9 — a composite's drawn lines sit on the same grid as the field over it
     the pane drew no leaves to measure
```

Run alone (`ONLY=composite`) it has not failed once in six consecutive runs.
Ten full runs on 2026-08-17 gave nine clean and one red; an earlier sitting saw
the same case red once in a full run and green alone.

The case is a real oracle and the assertion is doing its job — it exists
because *"every assertion below rides this list, so an empty one reports green
having measured nothing"* (`test/browser/cases.mjs:350-352`). What is wrong is
that the pane it measures is sometimes empty when it looks.

## Steps to reproduce

```
make browser-check          # repeat; expect a red run within ten
```

Each red run leaves its artifacts: `/tmp/glance-drive-*/9.png` and the page's
own log strip.

## Evidence

- The case: `test/browser/cases.mjs:325-352`. It opens the sheet over
  `drv-plan`, walks `n n` onto the whole-list composite, presses `RET`, and
  waits on **one** condition — `#dpara` carrying `on`
  (`cases.mjs:330-331`).
- The reading that comes up empty: `at.querySelectorAll(".de")` over
  `#mdoc .de.dat` (`cases.mjs:337`). So `.de.dat` was found — a composite row
  IS under the cursor and the edit IS open — and it carried no `.de` children
  at that instant.
- The neighbouring case waits on **two** conditions for exactly this reason:
  case 1 waits until the box is `on` *and* its height agrees with the block's,
  because *"`placeEdit' sizes the box a turn after the raise"*
  (`cases.mjs:40-47`).

## What it looks like

`#dpara.on` is set by the glue, and the Elm pane redraws the composite's leaves
on its own schedule. The case reads between the two: the box says it is open
before the leaves under it have been drawn. That would make the fix the same
one case 1 already has — wait on the *measurement* rather than on the class,
e.g. `until(() => document.querySelectorAll("#mdoc .de.dat .de").length > 0)`.

Unverified: nothing here rules out the leaves being drawn and then *removed*
for a frame, which would be a defect in the pane rather than in the case.
Whoever picks this up should confirm which by logging the leaf count across a
few frames after the raise before changing the wait.

## Not a regression

The case predates the `@` picker and the state mint; both were green in every
run above, red and clean alike. The rate did not move across either change.
