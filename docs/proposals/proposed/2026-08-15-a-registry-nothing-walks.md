# Proposal — a registry nothing walks looks exactly like one something walks

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/generalizer`,
cross-cut angle, over the whole tree.

## The measurement

`AGENTS.hs` holds registries as data — that is the file's doctrine, and
`test/TestSpec.hs` diffing them against the live code is what makes the doctrine
pay.  The measurement says how much of it pays:

- **59** top-level registries typed `[String]` / `[(String, String)]` / `[Path]`.
  **20 are named by `TestSpec.hs`.  39 are not, and 32 of those are referenced
  nowhere in the repo, including inside `AGENTS.hs` itself.**
- Across all **675** top-level bindings in `AGENTS.hs`, **550 are never named by
  the only test that reads the file.**
- Worst sections: *Sheets, document pane, Elm* — 8%, 89 values unread; *Keyword
  configuration* — 8%, 44 unread.

Growth is certain: the file was created 2026-08-13 and a registry-derivation
landed on 2026-08-15.

## The step nothing enforces

Adding a registry is two lines.  Wiring it to a test is a hand-written
`testCase`.  **Nothing distinguishes a wired registry from an unwired one** —
same type, same shape, same neighbourhood.  What that costs, measured:

- `AGENTS.hs:415` `poolCallers = ["Glance.Query.loadDirFilesWith", "app/Scan.hs"]`.
  `TestSpec.hs:238` asserts the same fact and **re-spells it by hand** as
  `["app/Scan.hs", "src-query/Glance/Query.hs"]` — a different spelling of the
  same two sites, with no join.  The registry has never been read.
- `AGENTS.hs:1010` `facadeExports`, three pairs.  `TestSpec.hs:508-513` asserts
  exactly those three pairs, hardcoded, without reading the registry.
- `AGENTS.hs:1121` `resolveSites`, four call sites; `AGENTS.hs:1244` then
  carries `Note "resolveIds has four call sites" [Unguarded]` — the same fact a
  **third** time, as prose admitting nothing checks it.
- `AGENTS.hs:2821` `captureCodes` and `scanCodes` are the same four strings six
  lines apart, neither read.  `scanCodes` is `[c | Code c _ <- captureCodes]`.
- `AGENTS.hs:718` `interopCases` (13) and `breaks` (7) duplicate
  `test/interop/drive.mjs:84`'s `BREAKS`.  The JS self-checks that a `BREAK`
  names a live case; the Haskell copy is checked by nothing, and `make interop`
  is outside `cabal test`, so it can never go red.
- For `savedViews` and `configSettings` the suite pins hand-typed literals
  against the **live** registries (`TestSpec.hs:611`, `:652`) — so code drift is
  red and **model drift is silent**.

## Proposed change

The registry that says which names the model holds and where the tree spells
them:

```haskell
data SiteKind = HsCall | HsExport | JsFn | JsConst | ElmPort | CssClass | CssVar | CaseName
  deriving (Eq, Show, Enum, Bounded)

-- | A name this repo writes twice: the model holds it, the tree spells it.
-- ONE list, folded by the suite, so a registry nothing walks cannot look like
-- a registry something walks.
data Sited = Sited { siWhat :: String, siKind :: SiteKind, siIn :: [Path] }

sited :: [Sited]
sited =
  [ Sited "mapFilesConcurrently" HsCall  ["app/Scan.hs", "src-query/Glance/Query.hs"]
  , Sited "derivedPath"          HsExport ["src-query/Glance/Query.hs"]
  , Sited "flagRow"              ElmPort ["frontend/elm/src/Listing.elm", "frontend/glue/20-sheet.js"]
  , … ]
```

One case in `TestSpec`, reusing the sweep already there (`TestSpec.hs:413-427`,
`sweptSources` + `callsIn`):

```haskell
testCase "every sited name is spelled where the model says" $ do
  gone <- concat <$> mapM absent sited
  assertEqual "a name the model holds and the tree does not spell" [] gone
```

**Then drop `-Wno-unused-top-binds` from `AGENTS.hs:3`.**  It exists to silence
these registries; once the fold uses them, an unused one becomes a compiler
warning for free.

## The sibling finding: three parallel enumerations, none enforced

`AGENTS.hs` has ten domain sections, each owning exactly one `*Notes` list and
exactly one `specGroupNN` **where NN is the section's ordinal**.  Three roll-ups
keep them in step by hand — `AGENTS.hs:4689` (`notes = concat […]`),
`TestSpec.hs:1736` (`spec = testGroup "Spec" [specGroup03..12]`), and
`test/Spec.hs:26-48` (21 modules).

With `-Wno-unused-top-binds` on and no export list, **a new `fooNotes` never
added to `notes` produces no warning, no compile error and no red test** —
`runghc AGENTS.hs` just under-reports the debt, and the debt count is the file's
entire deliverable.  Same for a `specGroup13`, and for a `TestFoo.hs` listed in
`glance.cabal` but never added to `Spec.hs` — it compiles and runs zero cases.
`Spec.hs`'s list has been edited in 20 commits, so member N+1 is certain.

```haskell
data Section = Section { secOrd :: Int, secName :: String, secNotes :: [Note] }
sections :: [Section]
notes = concatMap secNotes sections
```

with one case asserting `map secName sections` against the suite's group names.

## LOC

`Sited`: added ~45, removed ~60 now (six bespoke sweeps fold into one; the
duplicate `scanCodes` and the three-way `resolveSites` spelling collapse).
Sections: added ~14, removed ~6.  **Saved per future registry: 12 lines → 1, and
the default flips from silently-unchecked to checked.**

## Risk

Test-and-spec only.  No production module changes, no wire field moves, no org
bytes.  The one way it fails is `absent` mis-parsing a source file, which the
house `assertBool "too few … swept"` guard turns into a loud failure.

## Not a duplicate

`2026-08-15-the-skips-that-pass.md` is about the 116 `Unguarded`
**Notes** — prose.  `2026-08-13-verifiable-spec.md` proposes ID-tagging
prose in two files that no longer exist.  This is the same disease in a third
object, and the cheapest of the three: the registries are already Haskell values
in a module the suite already compiles.
