# Proposal — `+` adds a SIBLING of the stop, so an item joins the run the cursor stands in

**Status:** done — IMPLEMENTED 2026-08-11 · **Date:** 2026-08-11 · **Origin:** user — "`+` doesn't take
into account the current indentation level of cursor (which user modifies via
`f`/`b`) … `+` should be indentation-aware in lists and add an item in the last
bottom position of current indented block."

## Pattern

`+` in the materialize sheet's document pane adds a PARAGRAPH, always, and the
landing is `Scan.joinAt` (`assets/elm/src/Scan.elm:626-646`):

```elm
Para ->
    let up = outermost m r
    in Just ( up.id, up.to )
```

`outermost` (`Scan.elm:692-699`) walks the owner chain to its end, so a LEAF's
paragraph rides the composite the whole run sits in. Inside a list, `+` lands
past the WHOLE list; inside a nested run it lands past the outer list too,
having climbed two rungs (`ScanTest.elm:346-356`). The reason is written at the
docstring (`Scan.elm:616-625`) and in CLAUDE.md:1725-1727: grown in place, org
would close the list, cut the table, or take the prose for source.

That rule is indifferent to the grain, and the GRAIN is the one thing the reader
just spent keys on. `f` descends into a list's items and one rung further into a
nested run (`TestServe.hs:3728-3761`); `b` climbs back. Having navigated to a
nested item, `+` answers with a paragraph eleven lines below, past a structure
the reader was standing inside.

The four leaf shapes are not one thing, and today's rule treats them as one:

| the stop `f` landed on | `+` today | what it should be |
|---|---|---|
| a list ITEM | a paragraph past the whole list | an item at that item's indent, at its run's bottom |
| a NESTED item | a paragraph past the whole list | an item at the nested indent, at the nested run's bottom |
| a table LINE | a paragraph past the table | unchanged |
| a run inside `#+begin_X` | a paragraph past `#+end_X` | unchanged |

## Proposed change

**`+` ADDS A SIBLING OF THE STOP, and a sibling is a PREFIX plus the reader's
text.** The prefix — the `lead` — is what the new line opens with before a
character is typed: `""` for a paragraph, `"  - "` for an item two spaces in,
`"3. "` for the third entry of a numbered run, `"- [ ] "` where the stop wears a
checkbox. THE LEAD IS THE WHOLE OF THE NEW GRAMMAR, which is what bounds this
change: a run whose lines are not a prefix plus text gets no lead and keeps
today's landing.

That is why a TABLE LINE and a `#+begin_X` RUN are unchanged. A table row's
cells sit BETWEEN pipes, so no prefix spells one; a source line's grammar is
X's, and X is any word (`Scan.blockName`, `Scan.elm:167-186`) — the page holds
no org parser and must not grow one for a language it learns at read time.

`joinAt` answers a record rather than a pair, `ConfigParts`' own reason — three
values of two types, so a caller swapping two would compile:

```elm
type alias Join =
    { under : String   -- the row the new one is placed after
    , line : Int       -- the BODY line it takes
    , lead : String    -- what it opens wearing, "" for a paragraph
    }


joinAt : { a | rows : List Row } -> String -> Maybe Join
joinAt m id =
    case rowById m id of
        Nothing -> Nothing
        Just r ->
            case r.kind of
                Child -> Nothing
                Head  -> Just (Join r.id 1 "")
                Para  ->
                    case itemLead m r of
                        -- A SIBLING: the stop's own prefix, at the END of the
                        -- run the stop stands in.
                        Just lead ->
                            let last = lastSibling m r
                            in Just (Join last.id last.to lead)

                        -- Everything else rides its outermost owner, which is
                        -- the rule this one is carved out of.
                        Nothing ->
                            let up = outermost m r
                            in Just (Join up.id up.to "")
```

### 1. WHERE the run's bottom is, in `Scan`'s own model

`blocksIn` emits a list as `[whole, item1..itemN]` inline, each item's `up` at
its IMMEDIATE owner (`Scan.elm:347-481`, `pushItem` at `:353-393`): a top-level
item's owner is the list composite, a nested item's owner is the ITEM above it
(`ScanTest.elm:185-197` pins exactly this). So the RUN a stop stands in is
already spelled — it is the stops sharing its owner, and no re-scan of the lines
is owed:

```elm
{-| The RUN a stop stands in: the leaves sharing its owner, in document order.
-}
runOf : { a | rows : List Row } -> Row -> List Row
runOf m r =
    List.filter (\s -> s.kind == Para && s.grain == Leaf && s.owner == r.owner)
        m.rows


{-| Its LAST stop, which is the run's bottom — an item's own `to' already
covers the nested run drawn inside it, so the bottom is past those too.
-}
lastSibling : { a | rows : List Row } -> Row -> Row
lastSibling m r =
    Maybe.withDefault r (List.head (List.reverse (runOf m r)))
```

The landing LINE is that row's `to`, and the row it is placed AFTER is that row
— `joined` (`Scan.elm:742-767`) already walks past everything `under` owns, so a
last sibling carrying a nested run keeps it. "The last bottom position of the
current indented block" is `(lastSibling m r).to`.

`itemLead` answers `Just` only where the run has the grammar — the stop is a
LEAF and its outermost composite is named `list`:

```elm
itemLead : { a | rows : List Row, lines : List String } -> Row -> Maybe String
itemLead m r =
    if r.grain == Leaf && (outermost m r).name == Just "list" then
        Maybe.map (leadFrom m r) (listOpener (at r.from m.lines))
    else
        Nothing
```

### 2. WHAT the new item wears

**INDENT AND BULLET ARE THE STOP'S OWN; the LINE is the RUN's.** The two halves
come from two places on purpose: the reader chose the stop with `f`, and the ask
names the cursor's indentation — while a run can legally mix indents
(`listRun` opens a new item at any `o.indent <= base`, `Scan.elm:288-294`), so
the last sibling's indent is not necessarily the one the reader is looking at.

`listOpener` drops which bullet it saw today (`Opener = { indent : Int }`,
`Scan.elm:83-123`), so it grows a `bullet` field — the opener token plus the
horizontal run behind it, one space where the line ends at the bullet
(`"-"` alone opens an item, `ScanTest.elm:123-124`). Every existing reader takes
`.indent` and is untouched.

- **A NUMBER CONTINUES OFF THE LAST ITEM**, not off the stop: the item is being
  appended at the run's bottom, and the stop's own number spelled there is a
  duplicate. `1.`/`2.` gives `3. `; the punctuation is the STOP's (`.` or `)`),
  being the character the reader is looking at. A run whose last item is not
  numbered takes its length plus one. Org repairs a wrong number on the next
  structural edit either way — that is the argument for getting it right here
  rather than for leaving it, since the repair rewrites lines this page did not
  touch.
- **A CHECKBOX COMES ALONG EMPTY.** `- [X] alpha` gives `- [ ] `, org's own
  `org-insert-item`. It falls out of the prefix rule rather than being a second
  feature: `[ ] ` is part of what the line opens with. What it buys is the
  cookie — `[2/4]` counts boxes, so a box-less item joining a list of tasks
  silently changes the denominator's meaning.

### 3. The draft, and where the lead lives

`+` DRAWS THE ROW BEFORE IT IS WRITTEN (`d7ba44b`, `Scan.elm:601-654`), and that
survives: the drawn row wears the LEAD as its text, so the line the reader is
about to fill reads as the item it will be. It stays zero-width, grain
`Element`, id `D` — so `undrafted`, `placeOf` and the harness's `draft:para`
label are all unmoved (`Doc.elm:725-745`).

**THE LEAD IS THE ROW'S `was`**, which is one fact with two readers:

- `bodyText`'s `moved` test is `r.text /= r.was` (`Scan.elm:555-556`), so a
  draft whose text IS its lead has not moved and the splice passes it over —
  the drawn row still writes nothing, which is the rule the drafting exists
  under.
- `bodyText`'s zero-width arm calls `apart` to put the blank lines that keep a
  paragraph a paragraph of its own around it (`Scan.elm:580-592`, `:709-723`).
  An ITEM owes none: a blank above would push the run's own separator in front
  of a sibling, and the line below is whatever already followed the run. `was`
  is non-empty in exactly the cases that owe no blanks, so the arm reads
  `if r.was == "" then apart … else …` and no field joins `Row`.

`joinLine` (`Scan.elm:677-687`) reads the same fact: a paragraph's landing is
`line + 1` where a blank is written above it, an item's is `line` exactly.

`Doc.elm`'s `Draft`/`Insert` handlers (`:448-474`) are unchanged — they call
`drafted`, `insertion` and `joinLine`, all of which keep their signatures.

### 4. The composite grain still lands a paragraph, and that is the point

`b` from any item re-selects the whole list in one press
(`TestServe.hs:3747-3750`). Over the COMPOSITE stop, `+` lands a paragraph past
the whole structure — today's bytes, unchanged, and the ScanTest case that
pinned them moves from the leaf id to the composite id with the same expected
string (`ScanTest.elm:346-352` → the case in the plan below). **THE GRAIN IS THE
ASK'S OWN SELECTOR**: a reader who never presses `f` never meets a changed key,
and the old landing is one `b` away from the new one.

The ECHO says which, and the shell needs no grammar for it — `insertWord`
already climbs `owner` and reads the composite's `name`
(`assets/glue/20-sheet.js:188-197` at HEAD):

| stop | echo |
|---|---|
| the headline | `+ → org-insert-element (at the top)` |
| a paragraph | `+ → org-insert-element (after this paragraph)` |
| a list, a table, a block | `+ → org-insert-element (after the list)` … |
| a list ITEM, at any depth | `+ → org-insert-element (an item at this level)` |
| a table line, a block run | unchanged — the composite's own phrase |

### 5. The refusals

Standing, all of them:

- A CHILD refuses and names the door (`20-sheet.js:206-207`,
  `Scan.joinAt`'s `Child -> Nothing`, `TestServe.hs:3664-3669`).
- An id no row wears answers `Nothing` (`ScanTest.elm:384-386`).
- **A BLANK `+` WRITES NOTHING**, and the lead is what makes this load-bearing
  rather than tidy. The shell tests `!text.trim()` over the TEXTAREA
  (`20-sheet.js:384`), which holds what the reader TYPED — the lead never
  reaches it, living in `Scan` — so `+` then `RET` on an item writes no bare
  bullet and says `nothing added`.
- `ESC` leaves behind what it found: `undrafted` drops the lead-bearing row like
  any other (`Scan.elm:784-786`).

Nothing becomes newly reachable. Every leaf of a list composite has an opener on
its first line by construction — `pushItem` mints a leaf only at a line
`listOpener` answered for — so `itemLead` cannot fail its way past the guard.

## The invariant this revises

**ONE SENTENCE, and it is CLAUDE.md:1725-1727** (inside the `+` bullet at
`:1716-1733`):

> WHERE it lands is `Scan.joinAt`: a LEAF's rides its OUTERMOST owner, since org
> would close the list, cut the table or take the prose for source; the
> HEADLINE's leads the body at line 1.

becomes:

> WHERE it lands is `Scan.joinAt`, and THE GRAIN IS THE SELECTOR: a COMPOSITE's
> rides past the whole structure — one `b` from any item — while a LIST LEAF's
> joins THE BOTTOM OF ITS OWN RUN as an item wearing the stop's own PREFIX (its
> indent and bullet, an EMPTY checkbox where the stop has one, the run's next
> number where it is numbered). A TABLE line and a `#+begin_X` run keep the
> composite's landing, a pipe row and a source line being no prefix the page can
> spell. The HEADLINE's leads the body at line 1. THE LEAD IS THE DRAFT'S `was`,
> so the drawn row is still un-moved and still writes nothing, and a row wearing
> one owes `apart` no blank lines.

`docs/invariants.md` carries no entry for this rule — nothing to move there.

Two more copies of the sentence live in code and move with it: `joinAt`'s
docstring (`Scan.elm:616-625`) and the `insertion` describe-block comment
(`ScanTest.elm:344-345`).

## Test plan

### `assets/elm/tests/ScanTest.elm` — the landing rule, asked directly

`inserted id written lines` (`ScanTest.elm:94-101`) is the helper: it rewrites
the model the way `+`'s commit does and `bodyText` renders it. The typed text is
what the READER typed, so the lead appears in the expectation and never in the
argument — which is the assertion that the prefix is `Scan`'s.

Nine new cases, in the existing `insertion` describe (`:322-387`):

```elm
-- AN ITEM JOINS ITS RUN'S BOTTOM, wearing the stop's own prefix.
, test "an item's joins the END of its own run" <|
    \_ ->
        Expect.equal "* head\n- alpha\n- beta\n- note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "- beta" ]) [])
, test "the run's bottom, never the stop's own line" <|
    \_ ->
        Expect.equal "* head\n- alpha\n- beta\n- gamma\n- note"
            (Scan.bodyText
                (inserted "B1" "note" [ "* head", "- alpha", "- beta", "- gamma" ])
                []
            )
-- ONE BLANK STAYS INSIDE THE RUN (org's rule, `listRun'), so the bottom is
-- past it rather than at it.
, test "a blank line inside the run does not end it" <|
    \_ ->
        Expect.equal "* head\n- alpha\n\n- beta\n- note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "", "- beta" ]) [])
-- THE INDENT IS THE CURSOR'S: the nested run's own bottom, two spaces in.
, test "a nested item's joins the NESTED run, at the stop's indent" <|
    \_ ->
        Expect.equal "* head\n- alpha\n  - deep\n  - note\n- beta"
            (Scan.bodyText
                (inserted "B2" "note" [ "* head", "- alpha", "  - deep", "- beta" ])
                []
            )
-- AND AN OUTER ITEM'S BOTTOM IS PAST ITS OWN NESTED RUN, `joined' walking
-- everything the last sibling owns.
, test "an item carrying a nested run keeps it above the new sibling" <|
    \_ ->
        Expect.equal "* head\n- alpha\n  - deep\n- note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "  - deep" ]) [])
-- A NUMBER CONTINUES OFF THE LAST ITEM: the stop's own number spelled at the
-- bottom is a duplicate, which is what makes org renumber.
, test "a numbered run continues its numbering" <|
    \_ ->
        Expect.equal "* head\n1. alpha\n2. beta\n3. note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "1. alpha", "2. beta" ]) [])
, test "and the punctuation is the stop's own" <|
    \_ ->
        Expect.equal "* head\n1) alpha\n2) note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "1) alpha" ]) [])
-- A CHECKBOX COMES ALONG EMPTY, org's own `org-insert-item'.
, test "a checkbox item's sibling wears an empty box" <|
    \_ ->
        Expect.equal "* head\n- [X] alpha\n- [ ] note"
            (Scan.bodyText (inserted "B1" "note" [ "* head", "- [X] alpha" ]) [])
-- THE OLD LANDING IS ONE `b' AWAY, and these are the bytes it used to write
-- from the leaf.
, test "the COMPOSITE still lands a paragraph past the whole list" <|
    \_ ->
        Expect.equal "* head\n- alpha\n- beta\n\nnote\n\nafter"
            (Scan.bodyText
                (inserted "B0" "note" [ "* head", "- alpha", "- beta", "", "after" ])
                []
            )
```

Two existing cases are REVISED — they are the two the ask is about, and the
revision is the whole feature:

- `:346-352` "a list item's rides the whole list" — the id moves from the leaf
  `B2` to the composite `B0` (the case above), and the expectation is
  byte-identical, which is what says the composite grain did not move.
- `:353-356` "a nested item's climbs past its own item to the list" —
  `inserted "B2" "note" [ "* head", "- alpha", "  - deep" ]` now expects
  `"* head\n- alpha\n  - deep\n  - note"`.

Two existing cases STAND UNCHANGED and are the boundary of the change —
`:357-363` (a table's line rides the table) and `:364-373` (a block's run rides
the block). Their comments gain the reason: a pipe row and a source line are no
PREFIX.

Two more, in the `drafted` and landing groups:

```elm
-- The drawn ITEM writes nothing either: its text IS its `was'.
, test "the drawn item writes nothing at all" <|
    \_ ->
        let m = model [ "* head", "- alpha" ]
            rows = Maybe.withDefault m.rows (Scan.drafted m "B1")
        in
        Expect.equal ( 4, "* head\n- alpha" )
            ( List.length rows, Scan.bodyText { m | rows = rows } [] )
-- AN ITEM OWES NO BLANK, so its landing is the run's bottom exactly, where a
-- paragraph's is one line past the blank written above it.
, test "an item lands on the run's bottom line itself" <|
    \_ ->
        Expect.equal (Just 3)
            (Scan.joinLine (model [ "* head", "- alpha", "- beta" ]) "B1")
```

`ScanTest.elm:405-412` ("it stands under the WHOLE list, never between two
items") STANDS: for a flat run the last sibling is the last item, so the drawn
row's place in the row list is where it always was. What moved is the bytes, not
the drawing.

### `test/TestServe.hs` — the gesture, over `grainBody`

`grainBody` (`test/fixtures/shell-harness.js:174-191`) scans to
`head · para · comp:list · alpha · nested · beta · gamma · comp:quote · run ·
run · para · child` — indices 0…11, with `alpha` at 3 owning `nested` at 4
(`TestServe.hs:3695-3707`). The cursor keys are the fixture's own:
`press:Enter` opens the sheet on the head, each `press:n` skims one stop at the
element grain, `press:f` descends a rung.

**T1 — the echo and the drawn row, `n n f +` (cursor: head → lead-in → list →
alpha):**

```haskell
  , testCase "+ on an item draws the item it will be" $ do
      onTable "grain press:Enter press:n press:n press:f press:+" $ \answer -> do
        assertEqual "drawn at the run's bottom, where it always was"
                    [ "head", "para", "comp:list", "item", "item", "item", "item"
                    , "draft:para", "comp:quote", "item", "item", "para", "child" ]
          =<< map head <$> docOf answer
        assertEqual "wearing the stop's own bullet" ["- "]
          . partsOf "draft:para" =<< docOf answer
        assertEqual "and the cursor is on it" 7 =<< intAt "dat" answer
        echoIs "the echo names the level, not the structure"
               "+ \8594 org-insert-element (an item at this level)" answer
```

The row order and `dat` are `TestServe.hs:3590-3595` verbatim — a top-level
item's run ends where the list does, so the DRAWING is unmoved and only the
bytes and the lead are new.

**T2 — the write, `n n f +` then the text (REVISES `TestServe.hs:3640-3647`):**

```haskell
  , testCase "+ inside a list adds an item at the list's bottom" $
      onTable "grain press:Enter press:n press:n press:f press:+ dpara:note press:Enter" $
        \answer ->
          assertEqual "past gamma, inside the list, no blank line owed"
            [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma\n"
              <> "- note\n\n#+begin_quote\nquoted one\n\nquoted two\n#+end_quote\n\n"
              <> "tail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
```

**T3 — the ask itself, `n n f f +` (one rung further, onto `nested`):**

```haskell
  , testCase "+ on a nested item joins the NESTED run, at its own indent" $ do
      onTable "grain press:Enter press:n press:n press:f press:f press:+" $ \answer -> do
        assertEqual "drawn under the nested item, inside alpha"
                    [ "head", "para", "comp:list", "item", "item", "draft:para"
                    , "item", "item", "comp:quote", "item", "item", "para", "child" ]
          =<< map head <$> docOf answer
        assertEqual "wearing the nested indent" ["  - "]
          . partsOf "draft:para" =<< docOf answer
        assertEqual "and the cursor is on it" 5 =<< intAt "dat" answer
      onTable ("grain press:Enter press:n press:n press:f press:f press:+"
               <> " dpara:note press:Enter") $ \answer ->
        assertEqual "two spaces in, above the blank the outer run keeps"
          [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n  - note\n\n"
            <> "- beta\n- gamma\n\n#+begin_quote\nquoted one\n\nquoted two\n"
            <> "#+end_quote\n\ntail para\n** two\nchild body\n" ]
          =<< traverse (textAt "body") =<< listAt "writes" answer
```

**T4 — the old landing, one press up the grain, `n n +` (cursor on the list
composite):**

```haskell
  , testCase "the composite still lands a paragraph past the whole list" $ do
      onTable "grain press:Enter press:n press:n press:+ dpara:note press:Enter" $
        \answer ->
          assertEqual "the bytes `+' wrote from the leaf before the grain decided"
            [ "* TODO one\nlead in\n- alpha\n  more alpha\n  - nested\n\n- beta\n- gamma\n\n"
              <> "note\n\n#+begin_quote\nquoted one\n\nquoted two\n#+end_quote\n\n"
              <> "tail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
      onTable "grain press:Enter press:n press:n press:+" $
        echoIs "and the echo is the structure's, as it was"
               "+ \8594 org-insert-element (after the list)"
```

That expected string is `TestServe.hs:3644-3646` character for character — the
case moves from the leaf to the composite and asserts the same bytes.

**T5 — the checkbox, over `checkyBody`** (`shell-harness.js:195-201`: a
four-item list, `- [ ] alpha` / `- [X] beta` / `- [-] gamma` / `- delta`, whose
walk is `head · comp:list · four items`, so the keys are `Enter n f`):

```haskell
  , testCase "a checkbox item's new sibling comes along boxed and empty" $
      bootOf shell "" 500 ""
             "checky press:Enter press:n press:f press:+ dpara:epsilon press:Enter" $
        \answer ->
          assertEqual "an EMPTY box, whatever the stop's own state"
            [ "* TODO one\n- [ ] alpha\n- [X] beta\n- [-] gamma\n- delta\n"
              <> "- [ ] epsilon\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
```

**T6 — the blank refusal, which the lead makes load-bearing:**

```haskell
  , testCase "an empty + on an item writes no bare bullet" $
      onTable "grain press:Enter press:n press:n press:f press:+ press:Enter" $ \answer -> do
        assertEqual "nothing written" [] =<< textsAt "wroteAt" answer
        echoIs "" "RET \8594 org-ctrl-c-ctrl-c (nothing added)" answer
```

**T7 — the boundary, both UNCHANGED and green is the assertion:**
`TestServe.hs:3652-3662` (`+` inside a block lands under `#+end_`, keys
`n n n f +`) and `TestServe.hs:3664-3669` (`+` over a child refuses) stay as
written. A tabled case joins them, over `tabledBody`
(`shell-harness.js:209-221`, keys `Enter n n f` onto the first `|` line):

```haskell
  , testCase "a table's line keeps the composite's landing" $
      bootOf shell "" 500 ""
             "tabled press:Enter press:n press:n press:f press:+ dpara:note press:Enter" $
        \answer ->
          assertEqual "a pipe row is no prefix, so the paragraph goes past the table"
            [ "* TODO one\nlead in\n| a | b |\n|---+---|\n| 1 | 2 |\n| 3 | 4 |\n\n"
              <> "note\n\n- alpha\n- beta\n\ntail para\n** two\nchild body\n" ]
            =<< traverse (textAt "body") =<< listAt "writes" answer
```

### What neither suite can ask

Where the drawn lead SITS on screen — a drafted item at two spaces should line
up with the run it joins, and the harness returns zeros from every
`getBoundingClientRect` (`shell-harness.js:1289`). That is
`docs/proposal-browser-driver.done.md`'s class, and one case there would cover it.

## Files

- `assets/elm/src/Scan.elm` — `Opener` gains `bullet`; `Join`; `joinAt`,
  `runOf`, `lastSibling`, `itemLead`, `leadFrom`; `draftRow`, `joinLine` and
  `bodyText`'s zero-width arm read the lead.
- `assets/elm/tests/ScanTest.elm` — nine new cases, two revised, two recommented.
- `assets/glue/20-sheet.js` — `insertWord` alone (HEAD `:190-197`), six lines,
  off `grain`/`owner`/`name` the state push already carries.
- `test/TestServe.hs` — five new cases, one revised.
- `assets/elm.js` — `make elm` rebuild (a committed build input).
- `CLAUDE.md:1725-1727`, `CHANGELOG.md` (one Changed line).

`Doc.elm` is untouched: `drafted`, `insertion` and `joinLine` keep their
signatures, and the draft row keeps its id, its grain and its class
(`Doc.elm:725-745`), so the pane's markup and the harness's labels are unmoved.
Nothing in `src*/` or `app/` changes — the wire carries a body, and the body is
Elm's answer.

## LOC estimate

| | + | − |
|---|---|---|
| `Scan.elm` | 75 | 18 |
| `ScanTest.elm` | 75 | 8 |
| `TestServe.hs` | 65 | 8 |
| `20-sheet.js` | 6 | 2 |
| docs, CHANGELOG | 8 | 3 |

≈ **+229 / −39**. Marginal cost of the next prefix grammar (a definition list's
`- term ::`, say): one arm of `leadFrom` and one ScanTest case.

## Risk

- **`was` DOING TWO JOBS.** The lead is the draft's `was`, which is what makes
  the drawn row un-moved AND what tells `apart` a sibling from a paragraph. Any
  future path that sets `was` from the file for an inserted row breaks both at
  once, silently — `bodyText` would splice the lead twice or lose the blank
  line. The two ScanTest cases above (the drawn item writes nothing; the
  composite still gets its blanks) are the tripwire, and both bite.
- **A SECOND READING OF THE CHECKBOX.** `20-sheet.js:175`'s `CHECKBOX` regexp
  already restates `listOpener`'s bullet grammar for the `SPC` toggle; this adds
  a third spelling of `[ ]` in Elm. The two answer different questions (toggle
  vs. lead) and neither can be derived from the other today. It is a fold worth
  taking later, and naming it here is the whole of the mitigation.
- **A MIXED-INDENT RUN.** `listRun` admits an item at any indent `<= base`, so a
  run can hold both `  - a` and `- b`. The new item takes the STOP's indent at
  the RUN's bottom, so the reader can produce a run whose last two items sit at
  different indents. That is org-legal, is what the ask asks for, and is
  visible on screen the moment it happens.
- **ORG'S RENUMBER.** A run numbered `1. 1. 1.` — org's own accepted spelling —
  takes `2. ` at the bottom off the last item. Org repairs the run on its next
  structural edit; nothing here is wrong, and nothing here is what a reader of
  that file expected either.
- **No wire risk.** No route, no JSON field and no span math moves. A daemon
  serving an older `assets/elm.js` is unaffected — the file is compiled in.

## Existing precedent

- **THE GRAIN AS A SELECTOR** is already how three keys work: `RET` is pure edit
  at either grain, `d` flags whatever the stop is, and `f`/`b` are the only way
  to choose between them (CLAUDE.md's movement bullet, `TestServe.hs:3728-3767`).
  `+` reading the grain joins that set rather than starting it.
- **ONE RECORD, NOT POSITIONAL VALUES**: `ConfigParts` is a record "rather than
  three positional `Maybe Text` (all three the same type, so a caller swapping
  two would compile)". `Join` is that argument at three fields.
- **A PREFIX THE WRITER SPELLS AND THE READER FILLS** is `addTagEdits`' shape on
  the Haskell side — the span math writes `" :TAG:"` and the author's bytes are
  untouched around it.
- **THE OLD BEHAVIOUR STAYS REACHABLE** is `priority`'s: it left the default
  sort chain and stayed reachable as `sort:priority`, rather than being removed.
- **REVISING A STATED RULE, in the open**: `docs/proposal-generalize-capabilities.done.md`
  re-validated its own pattern and recorded that it had GROWN. This one says
  which sentence changes and why the reason behind it no longer covers the case.

## As built (2026-08-11)

Every expected string above held against the scanner — the nine ScanTest
expectations, the two revised ones, and all six `grainBody`/`checkyBody`/
`tabledBody` bodies — with no hand-computed byte corrected.

Three things the plan did not say:

- **`joinAt` and `drafted` widened to `{ a | rows, lines }`.** `itemLead` reads
  the stop's own LINE, so the record the landing is asked of carries the lines
  now. `Doc.Model` and `ScanTest.model` both already have them, so no call site
  moved.
- **`Opener` grows `bullet` through one token**, since `numberedAt` answers the
  DIGITS and the punctuation is the character behind them. `gapAfter` is the
  horizontal run, one space where the line ends at the token.
- **One more existing case revised than the plan named.** The leaf half of
  `TestServe.hs`'s "+ draws the empty paragraph, and point goes to it" asserted
  exactly T1's rows and `dat` under the comment "A LEAF'S stands past the WHOLE
  list" — the superseded rule. It folded into T1, which asserts those two plus
  the lead and the echo.

Open decision 1 is taken as recommended: a `#+begin_X` run keeps the composite's
landing. Decisions 2 and 3 stand where the plan left them.

## Open decisions

1. **WHETHER A `#+begin_X` RUN EVENTUALLY GETS ITS OWN LANDING.** The
   recommendation above is NO — a source line has no prefix, so `+` inside a
   block keeps landing past `#+end_X`. The counter-argument is real and is the
   ask's own: the reader pressed `f` to get INSIDE the block, and a new
   paragraph at the end of a `#+begin_quote` (this corpus's commonest block,
   `shell-harness.js:166-173`) is exactly what that gesture means. What stops it
   is that `quote` and `src` want different answers from one code path, and the
   page cannot tell them apart without learning what X means. **A human takes
   this one** — the deciding fact is how often the corpus's blocks are prose.
2. Whether the echo should name the DEPTH (`an item at level 2`) rather than
   `at this level`. The depth is `ownersOf`'s length and costs the shell one
   walk it already makes; the phrase is a taste call and a reader can see the
   indent.
3. Whether `+` on the LAST item of a run should ever land BETWEEN it and what
   follows, given the run's bottom and the stop coincide there. It does not
   here, and nothing distinguishes the two cases — noted because a reader
   standing on the last item may read the landing as "after this item" and be
   right by accident.
