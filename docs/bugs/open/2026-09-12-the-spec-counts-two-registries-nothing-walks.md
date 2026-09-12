# Bug — the spec counts two registries nothing walks

**Status:** open · **Reported:** 2026-09-12 (`runghc AGENTS.hs` reports
`keys 56 · popups 6`; the tree serves 59 and 8) · **Surface:** `AGENTS.hs`'s
`registryLine`, `Glance.Web.Keymap`, `Glance.Web.Page.Popups`

## Symptom

`runghc AGENTS.hs` prints a registry line claiming two counts the code does not
have:

- **keys — the model says 56, the shell serves 59.** Three bindings the page
  dispatches on are absent from the model, so the spec cannot be read as the
  keymap's contract.
- **popups — the model says 6, the page raises 8.** Two surfaces are absent, and
  a third's tier disagrees.

Both registries were counted, and neither has ever been compared to the code.
Everything is green: `cabal test`, `runghc AGENTS.hs` 0, `make browser-check`.

## Steps to reproduce

1. `runghc AGENTS.hs` — read the `registries — … keys 56 · popups 6 …` line.
2. Count the live rosters:
   - `keyBindings` (`src-web/Glance/Web/Keymap.hs:35-143`) — **59**. The two
     dialect comprehensions (`:40-46`) are 6 rows each, the four `aliased` calls
     2 rows each, the rest 1 apiece.
   - `popups` (`src-web/Glance/Web/Page/Popups.hs:36-48`) — **8** entries.
3. `grep -rn 'popTiers\|keyBindings\|Glance.Web.Keymap\|Page.Popups' test/` —
   **no hit.** Neither registry is reachable from any suite.

## Evidence

### Keys — 56 against 59

- `AGENTS.hs:4084-4146` — `bindings`, **56** `Binding` rows.
- `AGENTS.hs:6453` — `"keys " ++ show (length bindings)`, inside `registryLine`
  (`:6449-6457`).
- `src-web/Glance/Web/Keymap.hs:35-143` — `keyBindings`, **59** rows.
- The three the model does not carry are all MODAL-scope, and all three are
  sequences the model already spells at `table` scope, so the miss is a
  (sequence, scope) pair rather than a new key:
  - `Keymap.hs:121` — `bind ["X"] "org-glance-material:hide-done"
    (Just "hideDoneHere") "modal"`. Absent from `bindings` at any scope.
  - `Keymap.hs:114` — `bind ["C-c", "C-s"] "org-glance-overview:schedule"
    (Just "scheduleHere") "modal"`. `AGENTS.hs:4128` has the table-scope row
    only.
  - `Keymap.hs:116` — `bind ["C-c", "C-d"] "org-glance-overview:deadline"
    (Just "deadlineHere") "modal"`. `AGENTS.hs:4129` has the table-scope row
    only.
- The model already knows scope disambiguates a sequence —
  `scopedHelps` (`AGENTS.hs:4147-4151`) exists for exactly that and carries one
  entry, `(SModal, ["@"])`. Three more were owed and never written.
- `AGENTS.hs:4476` carries `Note "The modal surfaces' keys live outside
  keyBindings…" [Unguarded]` — prose admitting one gap, while this one goes
  unrecorded.

### Popups — 6 against 8

- `AGENTS.hs:4660-4666` — `popTiers`, **6** pairs: *state palette*, *tag
  manager*, *materialize sheet*, *link popup*, *capture form*, *settings sheet*.
- `AGENTS.hs:6454` — `"popups " ++ show (length popTiers)`.
- `src-web/Glance/Web/Page/Popups.hs:36-48` — `popups`, **8** entries:
  `modal`, `prompt`, `config`, `links`, `tags`, `capture`, `mint`, `refer`.
  - **`mint`** (`Popups.hs:46`) — `Popup "mint" "n" "nbox" Band True True` —
    absent from the model. It is the surface the popup-registry proposal was
    filed over (`docs/proposals/partial/2026-08-18-generalize-popup-surface-registry.md`),
    and `docs/invariants.md` names it: *"six sibling id lists were hand-edited
    and the seventh missed, so `#mint` neither faded nor dimmed."*
  - **`refer`** (`Popups.hs:47`) — `Popup "refer" "r" "rbox" Untiered False
    False` — absent. The model's `PopTier` (`AGENTS.hs:4658`) is
    `PopBand | PopSheet` and has **no third constructor** for a surface that
    hangs at the caret and takes no box, so `refer` cannot be added without
    widening the type.
  - **`capture`** disagrees on its tier: the model says `("capture form",
    PopSheet)` (`AGENTS.hs:4664`); the code says `Band` (`Popups.hs:45`), with
    the comment *"the band is the palette's own tier and the shape this form now
    has."* The model holds the retired shape.

### Neither is walked

- `test/TestSpec.hs:6` — the `import AGENTS (…)` list. It names `routes`,
  `viewColumns`, `flags`, `gluePartFiles`, `components` and thirty more, and
  neither `bindings` nor `popTiers`.
- No file under `test/` mentions `Glance.Web.Keymap`, `keyBindings`,
  `Glance.Web.Page.Popups` or `popTiers`.
- `AGENTS.hs:3` carries `-Wno-unused-top-binds`, so an unread registry is not
  even a warning — the general case is
  `docs/proposals/proposed/2026-08-15-a-registry-nothing-walks.md`, and these are
  two of its members caught diverging.

## Proposed fix

`TestSpec` already diffs three registries against their live twins — routes
(`TestSpec.hs:743`), commands (`:976`) and columns (`:849-852`, comparing
`Q.viewColumns` to `AGENTS.viewColumns` triple by triple). Add the two missing
diffs in the same shape:

```haskell
, testCase "every key the shell binds is a key the model holds" $
    assertEqual "a binding moved"
      (sort [ (unwords (bkeys b), scopeWord (bscope b), elispOf (bcmd b)) | b <- Spec.bindings ])
      (sort [ (T.unpack (T.unwords (kbKeys b)), T.unpack (kbScope b), T.unpack (kbCommand b))
            | b <- keyBindings ])

, testCase "every popup the page raises is a popup the model tiers" $
    assertEqual "a surface moved"
      (sort [ (n, t) | (n, t) <- Spec.popTiers ])
      (sort [ (T.unpack (puWrap p), tierOf (puTier p)) | p <- popups ])
```

Both need an export widened — `Glance.Web.Keymap` exports only
`keyBindingsJSON` (`Keymap.hs:3`), and `popTiers` keys its rows by a PROSE name
(*"state palette"*) rather than by the wrapper id the code uses (`prompt`), so
the model's rows must be re-keyed to `puWrap` for the diff to be a join rather
than a translation table.

Then correct the three facts the diffs would go red on: add the three modal
bindings to `AGENTS.bindings`, add `mint` and `refer` to `popTiers` (which needs
a third `PopTier` constructor for `refer`'s untiered box), and move the capture
form from `PopSheet` to `PopBand`.

`registryLine` then prints counts that are checked rather than asserted, which
is the file's whole deliverable.
