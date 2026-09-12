# Bug — a new git action runs no steps and answers ok

**Status:** open · **Reported:** 2026-09-12 (`/generalizer` sweep over the
git-sync UI; latent, no user-visible instance yet) · **Surface:** `POST
/git/sync`, `Glance.Web.Git.stepsFor`

## Symptom

A `SyncAction` constructor that is not one of the five git jobs and not one of
the three auto-sync pokes runs **zero git commands** and answers a 200 saying
the job succeeded:

```json
{"ok":true,"steps":[],"output":""}
```

The client reads that as done. `80-git.js` flashes the success class and
re-polls; nothing was fetched, committed or pushed.

The same fault reaches the five real jobs from the other direction: remove any
of `AutoOn`/`AutoOff`/`Arm` from the membership list at `Git.hs:223` and it
falls to `runSync`, where `stepsFor`'s wildcard gives it `[]` and the route
answers the same cheerful 200.

Latent as the tree stands: `syncActionOf` (`Git.hs:96-106`) maps exactly the
eight words, five of which `stepsFor` names and three of which the `elem` guard
catches. The defect is that nothing holds that arithmetic — no type, no test.

## Steps to reproduce

1. Add one constructor to `SyncAction` (`src-web/Glance/Web/Git.hs:93`), e.g.
   `… | Sync | Stash | AutoOn | …`.
2. Give it a word in `syncActionOf` (`:97-106`): `"stash" -> Just Stash`.
3. Build. **It compiles clean** — `stepsFor`'s `_ -> []` (`:154`) absorbs it and
   `handle`'s `otherwise` (`:231`) routes it to `runSync`.
4. `curl -s localhost:PORT/git/sync -d '{"action":"stash"}'`
5. Observe `{"ok":true,"steps":[],"output":""}` and an unchanged work tree.

Equivalently, with no source edit: delete `Arm` from the list at `:223`, then
`{"action":"arm"}` answers `ok` and arms nothing.

## Evidence

- `src-web/Glance/Web/Git.hs:93` — `data SyncAction = Fetch | Pull | Push |
  CommitPush | Sync | AutoOn | AutoOff | Arm`. One sum, two unrelated jobs: five
  step lists and three pokes at the auto-sync handle.
- `src-web/Glance/Web/Git.hs:147-154` — `stepsFor`, ending
  `_ -> []  -- AutoOn/AutoOff/Arm are the route's, never git steps.` The comment
  is the whole guard.
- `src-web/Glance/Web/Git.hs:129-133` — `runSync dir action = go (stepsFor
  action) (SyncResult True [] "")`, and `go [] acc = pure acc`. An empty step
  list returns the seed, which is `srOk = True`.
- `src-web/Glance/Web/Git.hs:223` — `| a `elem` [AutoOn, AutoOff, Arm] =` — the
  split done by hand-written membership.
- `src-web/Glance/Web/Git.hs:229` — `_arm -> autoSyncArm as`, a second wildcard
  nested inside the first branch: a fourth poke silently becomes `arm`.
- `src-web/Glance/Web/Git.hs:231-235` — the `otherwise` branch encodes
  `["ok" .= srOk r, "steps" .= srSteps r, "output" .= srOutput r]` with no floor
  on what ran.
- `src-web/Glance/Web/Git.hs:6-17` — the module export list. **`stepsFor` and
  `runSync` are both unexported**, so `test/TestGit.hs:8` cannot import them;
  the suite pins `actionFor` (`TestGit.hs:82-91`) and `syncActionOf` (`:93-100`)
  and nothing below them.
- `docs/invariants.md`, *Shape* — *"closed sums are matched one equation per
  constructor, no wildcard, so a new constructor is named by the compiler. A
  `_ ->` added for tidiness turns every future key, kind or frame into a silent
  default instead of a build error."* This is that, in the one module that was
  written after the rule.

## Proposed fix

The failing test first, which needs `stepsFor` exported:

```haskell
, testCase "every git action runs at least one git command" $
    assertBool "an action with no steps answers ok having done nothing" $
      all (not . null . stepsFor) [minBound .. maxBound]
```

It goes red the moment a stepless constructor exists, which is what the wildcard
hides today.

The structural fix is
`docs/proposals/proposed/2026-09-12-generalize-git-sync-states.md`: split the sum
into `SyncStep` (the five that run git) and `AutoSet` (the three that poke the
handle), joined at the door by `data GitPost = Step !SyncStep | Auto !AutoSet`.
`stepsFor :: SyncStep -> [[String]]` is then total with no wildcard, the `elem`
membership becomes a two-arm case, and `_arm` becomes three equations — so the
compiler names every site a ninth constructor touches.

A smaller patch, if the split is not taken: make `stepsFor` total by writing the
three `AutoOn`/`AutoOff`/`Arm` equations out, export it, and add the case above.
That closes the symptom and leaves the two hand-written splits standing.
