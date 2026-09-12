# Proposal — one sum per git job, and the client's mirror derived

**Status:** proposed · **Date:** 2026-09-12 · **Origin:** `/generalizer`, the
variant-cost sweep over the git-sync UI after the browser fixture landed.

## Pattern

**One sum holds two unrelated jobs.** `SyncAction`
(`src-web/Glance/Web/Git.hs:93`) is
`Fetch | Pull | Push | CommitPush | Sync | AutoOn | AutoOff | Arm`: five git
step lists and three route-only pokes at the auto-sync handle. Nothing in the
type says which is which, so three hand-written guards hold them apart:

- `stepsFor` (`Git.hs:147-154`) ends `_ -> []` with the comment
  *"AutoOn/AutoOff/Arm are the route's, never git steps."*
- `gitSyncRoute` splits on a hand list: `a `elem` [AutoOn, AutoOff, Arm]`
  (`Git.hs:223`).
- Inside that branch, a nested wildcard names the third: `_arm -> autoSyncArm as`
  (`Git.hs:229`).

The consequence is silent. A ninth constructor — `Status`, `Stash`,
`PullRebaseAll` — added to `SyncAction` and to `syncActionOf`
(`Git.hs:96-106`) compiles clean, takes the `otherwise` branch at `Git.hs:231`,
runs `runSync` over `stepsFor`'s `[]`, and `go [] (SyncResult True [] "")`
(`Git.hs:129-133`) answers `{"ok":true,"steps":[],"output":""}` — a 200 saying
the job ran. `stepsFor` and `runSync` are unexported (`Git.hs:6-17`), so
`TestGit.hs:8` cannot reach them and the suite pins only `actionFor`
(`TestGit.hs:82-91`) and `syncActionOf` (`:93-100`). Filed as
`docs/bugs/open/2026-09-12-a-new-git-action-runs-no-steps-and-answers-ok.md`.

This is the exact shape `docs/invariants.md` forbids: *"closed sums are matched
one equation per constructor, no wildcard, so a new constructor is named by the
compiler."*

**The state ladder is spelled twice, in two languages.**
`actionFor` (`Git.hs:109-121`) is a seven-branch guard chain from a `GitStatus`
to the one safe action. `frontend/glue/80-git.js:10` opens
`// Mirrors the backend's \`actionFor'.` and `glyphFor` (`:11-32`) re-spells the
same ladder in the same order, each branch also carrying the glyph, the count
text, the CSS class and the label:

| state | `actionFor` | `glyphFor` |
|---|---|---|
| detached / no upstream | `Nothing` (`:111`) | `⚠ g-detached`, action `null` (`:14`) |
| dirty, level | `CommitPush` (`:112`) | `● g-dirty`, `"commit-push"` (`:17`) |
| diverged | `Sync` (`:113`) | `↕ g-diverged`, `"sync"` (`:20`) |
| dirty | `CommitPush` (`:114`) | `● g-dirty`, `"commit-push"` (`:23`) |
| behind | `Pull` (`:115`) | `↓ g-behind`, `"pull"` (`:26`) |
| ahead | `Push` (`:116`) | `↑ g-ahead`, `"push"` (`:29`) |
| clean | `Fetch` (`:117`) | `✓ g-clean`, `"fetch"` (`:31`) |

A new state costs `Git.hs` + `80-git.js` + `assets/page.css:1421-1426` (one
colour rule per `g-*` class) + `TestGit.hs`, and **nothing compares the two
ladders**: the browser suite's two git cases assert the control mounts and
survives a re-apply (`docs/invariants.md`, *"the browser suite's git control has
a tree of its own"*), never that the glyph agrees with `actionFor`. Divergence
would show as the button offering `pull` where the server would `sync`.

`GET /git` already answers the raw counts (`statusJSON`, `Git.hs:237-247`) and
the page decides from them, which is why the ladder had to be copied.

## Files

`src-web/Glance/Web/Git.hs:93-106`, `:109-121`, `:129-154`, `:211-236`,
`:237-247`; `frontend/glue/80-git.js:10-32`; `assets/page.css:1421-1426`;
`test/TestGit.hs:82-100`.

## Proposed change

**1. Two sums, joined at the door only.**

```haskell
-- | A git job: a list of steps, run in order, stopping at the first failure.
data SyncStep = Fetch | Pull | Push | CommitPush | Sync
  deriving (Eq, Show, Enum, Bounded)

-- | A poke at the auto-sync handle.  No git runs.
data AutoSet = AutoOn | AutoOff | Arm
  deriving (Eq, Show, Enum, Bounded)

-- | What @POST \/git\/sync@ was asked for.  THE SPLIT IS THE TYPE'S, so the
-- route's two roads are a total case rather than a membership test.
data GitPost = Step !SyncStep | Auto !AutoSet
  deriving (Eq, Show)

stepsFor :: SyncStep -> [[String]]      -- total; no wildcard
stepsFor Fetch      = [["fetch"]]
stepsFor Pull       = [["pull", "--ff-only"]]
stepsFor Push       = [["push"]]
stepsFor CommitPush = [["add", "-A"], ["commit", "-m", "glance: sync"], ["push"]]
stepsFor Sync       = [["pull", "--rebase"], ["push"]]
```

`gitSyncRoute`'s `handle` becomes `case a of { Step s -> …; Auto x -> … }` and
the inner `_arm` wildcard becomes three equations.

**2. The word roster off `Enum`/`Bounded`,** the way `LinkPlace`
(`Query.hs:1770-1782`) does it — one word function, membership derived:

```haskell
postWord :: GitPost -> Text
postWord (Step Fetch)      = "fetch"
…
postWord (Auto Arm)        = "arm"

gitPosts :: [GitPost]
gitPosts = map Step [minBound .. maxBound] <> map Auto [minBound .. maxBound]

syncActionOf :: Text -> Maybe GitPost
syncActionOf w = listToMaybe [ p | p <- gitPosts, postWord p == w ]
```

A new step is then one constructor, one `stepsFor` equation the compiler demands
and one `postWord` equation the compiler demands. Nothing else.

**3. `GET /git` answers the decision, so the glue renders rather than decides.**
`statusJSON` gains three members off `actionFor`:

```haskell
, "action" .= fmap postWord (Step <$> actionFor s)   -- null where nothing is safe
, "glyph"  .= glyphOf s                              -- "✓" | "●" | "↕" | "↓" | "↑" | "⚠"
, "cls"    .= classOf s                              -- "g-clean" | … , the CSS class
, "label"  .= labelOf s                              -- the tooltip sentence
```

`80-git.js:11-32` deletes `glyphFor` and reads `status.action`, `status.glyph`,
`status.cls`, `status.label`. The count text (`n`) stays client-side: it is
formatting over the same numbers the payload already carries.

**4. `TestGit` pins the table once.**

```haskell
testCase "every state names its step list, its glyph and its label" $
  assertEqual "the git ladder moved"
    [ (clean, Just "fetch", "✓", "g-clean"), (dirty, Just "commit-push", "●", "g-dirty"), … ]
    [ (s, decided (porc s)) | s <- everyPorcelain ]
, testCase "every step runs at least one git command" $
    assertBool "a step with no steps answers ok" $
      all (not . null . stepsFor) [minBound .. maxBound]
```

The second case is the bug's regression test and needs `stepsFor` exported.

## LOC estimate

Added ~55 (`SyncStep`/`AutoSet`/`GitPost`, `postWord`, `glyphOf`/`classOf`/
`labelOf`, the widened `statusJSON`, two `TestGit` cases). Removed now ~45
(`glyphFor`'s 22 lines of JS, the `elem` guard, two wildcards, the hand-written
`syncActionOf` cascade). **Per future state: one Haskell constructor plus one
`page.css` colour rule — down from four files, and the compiler names every site
instead of a reviewer.** Per future *action* (a step with no new state): one
constructor and one `stepsFor` equation, both compiler-demanded.

## Risk

- **API**: `SyncAction (..)` is exported (`Git.hs:9`) and imported by
  `TestGit.hs:8`; the constructors move under `SyncStep`/`AutoSet`. In-tree
  only — the type is not on any public surface.
- **Wire**: `GET /git` GAINS four members and loses none, so the glue's
  fallbacks keep working during the change. `POST /git/sync`'s accepted words
  are unchanged; `postWord` reproduces `syncActionOf`'s eight strings exactly,
  and `TestGit.hs:93-100` is what proves it.
- **On-disk**: none. No org bytes, no git command changes.
- **Test baselines**: `TestGit.hs` grows two cases. The browser git cases
  (`cases.mjs`, the two `repo: true` ones) read `#gitctl`'s rendered text — a
  glyph that moves moves them, which is why part 3 must keep the six glyphs
  byte-identical to `glyphFor`'s. `BREAK=ghead-row` (`drive.mjs:160`) still
  turns the mount case red.

## Existing precedent

- `CommandKind` (`Commands.hs:131-138`): `Splices`/`Makes`/`Moves` carry their
  payloads in the constructor, so the dispatch and the id-arity wall are read
  off the kind rather than off the verb's name.
- `LinkPlace` (`Query.hs:1770-1782`): a closed word over `[minBound ..
  maxBound]`, one `*Word` function, the arg wall and both roads reading one
  vocabulary.
- `docs/invariants.md`: *"closed sums are matched one equation per constructor,
  no wildcard"* and *"a
  fact several readers agree on is spelled in ONE list, indexed by key."*
