# Proposal — three kinds of invariant, one checker each, and a registry that says which is enforced

**Status:** proposed · **Date:** 2026-08-13 · **Origin:** user, asking what a
compact verifiable spec for this project would look like and whether it wants
Prolog, Haskell or something else.

## The measurement that decides it

| | Lines |
| --- | --- |
| `CLAUDE.md` | 2608 |
| `docs/invariants.md` | 4911 |
| **invariant prose, total** | **7519** |
| `testProperty` call sites | 30 (23 `TestProperties`, 7 `TestDefaults`) |
| structural sweeps | 252 lines, ~6 `testCase`s (`TestSelfContained`) |

And the number nobody in this repo can produce: **how many of those 7519 lines
have an enforcer at all.** Not an estimate — there is no mechanism that could
answer it, because a rule and the check that holds it are joined by nothing but
a reader's memory.

Evidence the join has already come apart: `CLAUDE.md`'s parity section cites
`fixtures/parity/sort-tokens.json` as the mechanism running the shared sort
grammar over the browser renderer. A `find` over this repo returns no path
matching `*parity*`. Either the fixture lives in the sibling renderer repo — in
which case the citation names a location this repo cannot check — or it is
gone. Both readings are one defect: **a stated enforcer that nothing
verifies is a stated enforcer, and reads exactly like a real one.**

That is the same failure this repo already found in its own proposal names and
already fixed, by checking the name against the header rather than keeping the
two in step by hand.

## The wrong axis

"Prolog or Haskell" picks one formalism for a set that has three shapes. The
invariants split cleanly, and each part has an obvious checker that the other
two parts would fight:

| Kind | What it claims | Examples from `CLAUDE.md` | Share | Checker |
| --- | --- | --- | --- | --- |
| **Law** | algebra over pure functions | `hsFull` is a left fold; decompose → recompose is byte-identical; `applyEdits` accepts exactly the disjoint sets; filter/renderer parity | ~60% | generators (Haskell) |
| **Structure** | facts about the code itself | five write sites all leave through `Watch.writeSpans`; `glance-web` layering runs one way; `wrappedWidgets` must-not-appear; `gluePartFiles` ↔ `jsconfig.json` | ~25% | Datalog / SQL over code facts |
| **Protocol** | temporal, multi-actor | the drain loop is serial so reseed reverts nothing; cursor+digest survives a union merge; rotation between an open and its write; mailbox overflow → `resync` | ~10% | TLA+ |

The remaining ~5% is taste and history — the register of the prose, the
superseded designs — and wants no checker.

Laws already have their checker as of `../done/2026-08-11-property-tests.md`.
Structure has ~6 hand-written sweeps and no way to add the seventh cheaply.
**Protocol has nothing**, and it is where the prose is doing the most work.

---

## Layer 0 — the registry

The compact part, and the spine. One list, the repo's own doctrine: ORDER IS
DATA, read twice.

```haskell
-- | ONE LIST, and every checker reads it.  A rule's ID is the SECOND place the
-- rule is written — the prose cites it, the check names it — so the sweep
-- compares two independent spellings rather than trusting either.
data Kind = Law | Structure | Protocol
  deriving (Eq, Show, Enum, Bounded)

data Invariant = Invariant
  { invId    :: Text    -- ^ @SPAN-1@, stable for the rule's life
  , invKind  :: Kind
  , invSays  :: Text    -- ^ one line, the universal as the prose states it
  , invCites :: [Text]  -- ^ where the prose says it
  }

invariants :: [Invariant]
invariants =
  [ Invariant "SPAN-1"  Law
      "hsFull is a left fold of <> over spanParts, seeded with hsStars"
      ["CLAUDE.md#spans", "docs/invariants.md:36"]
  , Invariant "EDIT-2"  Law
      "applyEdits accepts exactly the disjoint span sets"
      ["CLAUDE.md#commands", "src/Data/Org/Edit.hs:68"]
  , Invariant "WRITE-1" Structure
      "every write site leaves through Watch.writeSpans"
      ["CLAUDE.md#architecture"]
  , Invariant "DRAIN-1" Protocol
      "the drain loop is serial, so a reseed cannot revert an edit that landed during the walk"
      ["CLAUDE.md#architecture"]
  ]
```

A check names its rule in the one place a failure already prints:

```haskell
testProperty "[SPAN-1] hsFull ends at the last present part, never a maximum" $ ...
```

The prose gains one tag:

```markdown
- `hsFull` is derived, never stored: a left fold of `<>` over `spanParts`
  seeded with `hsStars`. [SPAN-1]
```

And the sweep closes both directions, because either alone rots in silence:

```haskell
-- An ID with no enforcer is prose claiming to be checked.  An enforcer naming
-- a dead ID is a check whose rule nobody can find.  Neither is visible from
-- the other end, so both are asked.
, testCase "every invariant has an enforcer, every enforcer a live invariant" $ do
    let declared = map invId invariants
    enforced <- enforcerIds        -- the [ID] tags across test/ and the rule files
    assertBool "the sweep found no enforcers at all" (length enforced >= 30)
    assertEqual "an invariant nothing enforces"  [] (declared \\ enforced)
    assertEqual "an enforcer naming no invariant" [] (enforced \\ declared)
```

Plus one report, which is the deliverable:

```
$ cabal run invariants -- report
  Law        31 declared   31 enforced
  Structure  12 declared    6 enforced   ← ARCH-2 ARCH-3 WALK-3 GLUE-2 CFG-1 CFG-4
  Protocol    7 declared    1 enforced   ← DRAIN-1 CURSOR-1 CURSOR-2 ROT-1 WS-1 WS-2
```

**First cut does not register all 7519 lines.** It registers the ~40 rules
already written as universals. The registry's value is the ratio moving and
being visible, never completeness on day one.

---

## Layer 1 — laws: Haskell, and mostly landed

Generators shipped 2026-08-11; `TestGen.hs` is 694 lines. What is left is the
one law with two implementations and no shared oracle: **parity**.

Today the contract is prose ("there is NO schema revision mechanism… agreement
rests on the port being kept term for term"), plus a runtime tripwire that
"reports a suspicion and corrects nothing". Make the Haskell side the oracle
and let it emit the corpus:

```haskell
-- test/GenParity.hs — writes the fixture the renderer replays.  One producer,
-- so a divergence is FOUND rather than blessed by whoever last read both.
main = do
  rows    <- sample 200 genRow      -- TestGen's own row generator
  queries <- sample 400 genQuery    -- every token shape Filter's grammar admits
  encodeFile "fixtures/parity/filter.json"
    [ Case q (rowId r) (matches (compile q) r) | q <- queries, r <- rows ]
```

```js
// test/browser — replay only.  No generator on this side, or there would be
// two spec authors again.
for (const c of cases) {
  const got = tokenTest(parseQuery(c.query), rowOf(c.row));
  if (got !== c.expected && !c.blessed) fail(`${c.query} @ ${c.row}: js ${got}, hs ${c.expected}`);
}
```

The seven divergences `CLAUDE.md` lists by hand become rows that a run counts,
and each declares its **direction** rather than being exempted:

```json
{ "query": "priority:A", "row": "r17", "expected": true,  "blessed": "renderer-narrower" }
{ "query": "ref:abc123", "row": "r04", "expected": true,  "blessed": "producer-only" }
```

Losing a blessed divergence by accident then goes red, which is the half the
current tripwire cannot do.

---

## Layer 2 — structure: where Prolog genuinely earns its place

These are joins plus negation-as-failure over facts about the code. Exactly
Datalog's shape, and exactly what a hand-written `testCase` is worst at.

Facts come from the compiler, not from grep: build with `-fwrite-ide-info`,
index the `.hie` files with **hiedb**, which is already a SQLite database of
Haskell definitions and references. Then each invariant is one rule:

```prolog
% [WRITE-1] every splice leaves through the watch's own door.
violation('WRITE-1', F) :-
    calls(F, 'Data.Org.Edit.replaceSpans'),
    F \= 'Glance.Web.Watch.writeSpans'.

% [ARCH-2] inside glance-web the dependency runs ONE way.
layer('Glance.Web.Base', 0).   layer('Glance.Web.Page', 2).
layer('Glance.Web.Routes', 3). layer('Glance.Web', 4).
violation('ARCH-2', M) :- imports(M, N), layer(M, I), layer(N, J), J >= I.

% [WALK-3] one name list serves the walk's denylist and isCanonical.
violation('WALK-3', N) :- denied(N), \+ canonicalExcludes(N).
```

Cost comparison, measured: `TestSelfContained` is 252 lines for ~6 sweeps. The
same six as rules is ~20 lines, **and the seventh costs one line instead of a
new `testCase`.** That is the whole argument — not elegance, the marginal cost
of the next invariant.

**Zero-new-language variant, try this first.** hiedb *is* SQLite, so the rules
can be SQL and the checker is one `sqlite3` call in the Makefile:

```sql
-- [WRITE-1]
SELECT caller FROM refs WHERE callee = 'replaceSpans'
  AND caller <> 'writeSpans';
```

Souffle only if the transitive rules (import closure) get uncomfortable —
`WITH RECURSIVE` covers them, so probably never.

**Honest limit:** hie covers Haskell. The glue's `wrappedWidgets` lists and the
Elm half stay textual. They already are, they are already cheap, leave them.

---

## Layer 3 — protocol: four small TLA+ models

Where a test can only sample an interleaving and the prose is asserting over
all of them. Drain-vs-reseed, in ~40 lines of PlusCal:

```
--algorithm drain
variables queue = {}, store = [f \in Files |-> Empty];

process (Watcher = "w")
begin W: while TRUE do
  with f \in Files do queue := queue \union {f} end with;
end while; end process;

process (Drain = "d")
begin D: while TRUE do
  either                                     \* settle one ripe path
    with f \in ripe(queue) do
      queue := queue \ {f};                  \* OUT of the queue before settling
      store[f] := parse(f);
    end with;
  or                                         \* config reseed
    R: store := walkAll();                   \* built OUTSIDE the transaction
  end either;
end while; end process;

\* [DRAIN-1] every edit is eventually visible — nothing a walk overlapped is lost.
Safety == \A f \in Files : edited(f) ~> (store[f] = parse(f))
```

`tlc` with 3 files and 2 writers decides it in seconds. `CLAUDE.md` states the
negative — "make the loop concurrent and any edit that landed during the walk
is silently reverted" — which is a claim about a program that does not exist,
and a model is the only cheap way to hold a claim like that true.

The other three, each the same size:

| Model | The claim | What samples it today |
| --- | --- | --- |
| `CURSOR-1` | offset + 2 sha1s survive a union merge inserting lines ahead of the cursor | one interop case, one interleaving |
| `ROT-1` | a rename between `appendLine`'s open and its write loses no line | prose only |
| `WS-1` | a full mailbox closes `resync` and drops no committed write | prose only |

Out of `cabal test`, own make target, skips loudly when `tlc` is missing —
`make interop`'s own pattern and for its own reason.

---

## Not proposed

**LiquidHaskell.** The fit is real and seductive: spans are half-open, nested,
ordered and non-overlapping, which is refinement types by construction. The
cost is wrong — it pins the GHC version this repo is currently moving under for
the WASM work, it annotates modules that carry no such burden today, and the
eight span laws got generators three days ago. Revisit only if spans regress.

**A bespoke spec DSL.** Compact to design, unrun within a year. Each of the
three formalisms above is already somebody else's maintenance problem.

---

## Order and cost

| Step | Cost | Buys | Blocked by |
| --- | --- | --- | --- |
| Registry + tag the 30 properties | ~1 day | the coverage number | nothing |
| Parity fixture generator | ~2 days | the largest live risk goes red on divergence | `TestGen` (present) |
| hiedb facts, port the 6 sweeps | ~2 days | the next structural invariant costs one line | `-fwrite-ide-info` in `glance.cabal` |
| Four TLA+ models | ~3 days | protocol claims held over every interleaving | nothing |

Step 1 stands alone and is worth doing even if nothing after it is.

## What it costs, honestly

- **The registry is a second place every rule is written.** That is the
  mechanism, not a side effect — the repo already pays this for proposal names
  and gets the check in return — but an ID rename is a real sweep across prose
  and tests.
- **Datalog would be a fourth language** in a repo already carrying Haskell, JS
  and Elm. The SQL variant avoids it; try that first and only escalate on the
  recursive rules.
- **A model is not the code.** A TLA+ model that holds proves the design and
  says nothing about the implementation under it. It catches the class the
  prose is worst at and no test samples, which is the whole of its claim.
- **The first coverage report will read badly** — 7519 lines against ~40
  enforcers. That number is the deliverable. Being unable to compute it would
  be the embarrassment.
