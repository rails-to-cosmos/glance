# Proposal — one list of popup surfaces, many readers

**Status:** partial · **Date:** 2026-08-18 · **Origin:** `/generalizer` audit,
weighed against the code it names.

## The finding, in one line

`git show d11f1ea` — the mint commit — hand-edited **six** sibling id lists and
missed the seventh. `#mint` neither transitions nor dims because of it.

## The pattern

The shell carries eight popup surfaces (`frontend/glue/70-shell.js:17-44`: mint,
prompt, refer, capture, links, tags, sheet, config). Every other reader spells the
membership by hand, and the copies have drifted:

| where | names | short by |
| --- | --- | --- |
| `Style.hs:79`, `:83` (veil) | 7 wrappers | `#refer`, which opts out at `:343` deliberately |
| `Style.hs:84` (boxes) | 8 | — complete |
| `Style.hs:271`, `:272`, `:273` | 6 / 5 / 7 | heads and feet, each its own list |
| `Style.hs:360` (transition) | `#app` + 6 | **`#mint`** |
| `Style.hs:361-362` (`html.stale` wash) | `#app` + 6 | **`#mint`** |
| `TestServe.hs:4966-4967` (`tiers`) | 5 | `#kbox`, `#nbox` — both wear tiers and are never swept |
| `AGENTS.hs:3230` `surfaces` | 6 | mint, refer |
| `AGENTS.hs:3432` `washCovers` | 4 | stale by three against the CSS it describes |
| `AGENTS.hs:3628` `popTiers` | 6 in prose | mint |

`grep -rn 'Spec.surfaces|washCovers|popTiers' test/ src/ src-web/ app/ frontend/`
exits 1: the three spec lists have no reader at all, so nothing goes red when they
rot.

`TestServe.hs:5047-5050` pins the stale-wash selector as a verbatim string, which
makes the six-surface list **sticky**: editing the CSS turns the suite red, and the
test can never report that the list is short.

## The change

A registry in a new `Glance.Web.Page.Popups`, in the idiom of `Main.flags`
(`app/Main.hs:191-204`) and `Query.viewColumns`:

```haskell
data Tier = Band | Sheet | Untiered   -- ^ `refer' hangs at the caret and takes none.

data Popup = Popup
  { puName   :: !Text   -- ^ the wrapper id, and the `?page=' name.
  , puPrefix :: !Text   -- ^ the letter every part id wears: p, l, t, c, k, n.
  , puTier   :: !Tier
  , puVeiled :: !Bool   -- ^ takes the backdrop, so it joins Style.hs:79 and :83.
  , puWashed :: !Bool   -- ^ dims with the page, so it joins :360 and :361.
  }

popups :: [Popup]
```

Then:

- `Style.hs`'s seven selector strings become joins —
  `sel [ "#" <> puName p | p <- popups, puVeiled p ]`;
- `Page.hs`'s `popupFrame` takes its tier off the row rather than a literal class;
- one `TestServe` case folds the registry in place of the literal `tiers`, which
  is what makes `#kbox` and `#nbox` swept for the first time;
- `AGENTS.hs`'s `surfaces` / `popTiers` / `washCovers` collapse into one table,
  bound by a `TestSpec` case the way `Spec.routes` and `Spec.cmds` already are.

The shell's `SURFACES` keeps its closures — they are behaviour — but its NAME
half can be asserted against the served config blob, which
`Glance.Web.Page.Glue.glueConfig` already ships `dcells`, `lcols` and `tcols`
through for exactly this reason. A surface in one and not the other then goes red.

## LOC

Added ~30 (the record, eight rows, four selector joins); removed ~14 of literal
selector string plus three dead spec lists. **The win is the next popup:** one
row instead of nine registration sites, six of which fail silently.

## Risk

`src-web` only, plus one test list. No wire field moves, no org bytes. The emitted
CSS must come out byte-identical for the surfaces that are correct today — the
verbatim pins at `TestServe.hs:5047-5050` already enforce that, and they are what
will catch a mistake in the join.

## Precedent

`design-rhymes.md:94-99` states the rule this proposal is an instance of: *"One
list, many readers… When prose says 'kept in sync', the design owes a
derivation."* The JS half is already a registry with nine readers.
