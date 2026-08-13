module ScanTest exposing (suite)

{-| THE SCANNER, ASKED DIRECTLY. Every case here is a rule the pane's behaviour
rests on that costs a booted page to reach through the Haskell suite — org's
one-blank-line rule, a block closing by NAME, an indented `*` being an item
where a column-1 one is a headline, and the splice's "one grain speaks for a
range".

The body always opens with a headline line, because `blocksIn` starts at index
1: the line the entry wears is the sheet's headline row, never a paragraph.

-}

import Array
import Expect
import Scan exposing (Grain(..), Kind(..), RegionKind(..), Row, blank)
import Test exposing (Test, describe, test)


{-| A body as the sheet holds it: lines, and how many of them are this entry's
own rather than a child's.
-}
scan : List String -> List ( Int, Int, String )
scan lines =
    List.map (\b -> ( b.from, b.to, grainOf b.grain ++ Maybe.withDefault "" (Maybe.map ((++) ":") b.name) ))
        (Scan.blocksIn (Array.fromList lines) (List.length lines))


scanOwn : Int -> List String -> List ( Int, Int, String )
scanOwn own lines =
    List.map (\b -> ( b.from, b.to, grainOf b.grain ++ Maybe.withDefault "" (Maybe.map ((++) ":") b.name) ))
        (Scan.blocksIn (Array.fromList lines) own)


{-| The leaf ownership, which is the grain LADDER: `up` is the IMMEDIATE owner.
-}
owners : List String -> List (Maybe Int)
owners lines =
    List.map .up (Scan.blocksIn (Array.fromList lines) (List.length lines))


grainOf : Grain -> String
grainOf g =
    case g of
        Element ->
            "element"

        Composite ->
            "composite"

        Leaf ->
            "leaf"


indentOf : String -> Maybe Int
indentOf line =
    Maybe.map .indent (Scan.listOpener line)


{-| A model the splice can be asked of: the rows a body scans to, over the lines
they came from.
-}
model : List String -> { rows : List Row, lines : List String }
model lines =
    { rows = Scan.rowsFrom lines (List.length lines) [] []
    , lines = lines
    }


{-| The same, with ROW's text rewritten — what an edit does before a flush.
-}
edited : String -> String -> List String -> { rows : List Row, lines : List String }
edited id written lines =
    let
        m =
            model lines
    in
    { m
        | rows =
            List.map
                (\r ->
                    if r.id == id then
                        { r | text = written }

                    else
                        r
                )
                m.rows
    }


{-| WHAT EVERY `d`/`D` LEAVES: the body each stop's own deletion composes, one
per stop in row order. A stop the pane draws is a stop a reader can take, so a
fixture asks about ALL of them rather than about the one that was reported.
-}
takes : List String -> List String
takes lines =
    let
        m =
            model lines
    in
    List.map (\r -> Scan.bodyText m [ r.id ])
        (List.filter (\r -> r.kind == Para) m.rows)


{-| THE BODIES THAT DO NOT PAIR UP, out of every one a `d`/`D` composes. An
opener left without its closer is what a phantom stop's deletion leaves behind,
and it is what a stop set alone cannot say.
-}
unpaired : List String -> List String
unpaired bodies =
    List.filter (not << pairsUp) bodies


{-| A stop's own text, which is what the pane draws inside it.
-}
textOf : String -> List String -> String
textOf id lines =
    List.foldr
        (\r acc ->
            if r.id == id then
                r.text

            else
                acc
        )
        ""
        (model lines).rows


{-| A LINE COUNT, never org's parser, and the name says only what it checks:
every `#+begin_` has a `#+end_` and every drawer opener an `:END:`. It is blind
to the NAMES matching, to ordering, and to what a line landed in the middle of —
`tableRuns` is the second reading beside it, and the rest is asserted case by
case.
-}
pairsUp : String -> Bool
pairsUp body =
    let
        lines =
            List.map (String.toLower << String.trimLeft) (String.split "\n" body)

        spells word =
            List.length (List.filter (String.startsWith word) lines)
    in
    (spells "#+begin_" == spells "#+end_")
        && (List.length (List.filter drawerOpens lines) == spells ":end:")


{-| A drawer's opener, org's charset and all — EVERY drawer rather than
`:LOGBOOK:` alone. Spelled again here so the oracle is the suite's own reading
rather than the code it is judging.
-}
drawerOpens : String -> Bool
drawerOpens line =
    let
        body =
            String.trim line

        inner =
            String.slice 1 (String.length body - 1) body
    in
    String.startsWith ":" body
        && String.endsWith ":" body
        && String.length body > 2
        && String.all (\c -> Char.isAlphaNum c || c == '-' || c == '_') inner
        && inner
        /= "end"


{-| HOW MANY TABLES A BODY SPELLS, counted as RUNS of pipe rows. A blank line —
or a bullet, or a source line — landing between two rows ENDS the table and opens
another, which is org's own `[1 table] -> [2 tables]`. A second independent
reading beside `pairsUp`, and the one that catches a marker no block ever lost a
closer over.
-}
tableRuns : String -> Int
tableRuns body =
    String.split "\n" body
        |> List.foldl
            (\line ( n, inside ) ->
                if String.startsWith "|" (String.trimLeft line) then
                    ( if inside then
                        n

                      else
                        n + 1
                    , True
                    )

                else
                    ( n, False )
            )
            ( 0, False )
        |> Tuple.first


{-| The REGION holding LINE of a body, as its kind and its extent.
-}
regionOf : Int -> List String -> ( String, Int, Int )
regionOf line lines =
    let
        reg =
            Scan.regionAt (Array.fromList lines) 1 (List.length lines) line
    in
    ( regionWord reg.kind, reg.from, reg.to )


{-| And what that region says a new line inside it opens with.
-}
markerOf : Int -> List String -> String
markerOf line lines =
    Scan.markerFor (Array.fromList lines) (Scan.regionAt (Array.fromList lines) 1 (List.length lines) line)


regionWord : RegionKind -> String
regionWord k =
    case k of
        Plain ->
            "plain"

        Item ->
            "item"

        Table ->
            "table"

        Block ->
            "block"

        Drawer ->
            "drawer"


{-| EVERY CARET IN A BODY, one entry per line: the region holding it, that
region's extent, and the marker it spells. A fixture states the whole walk in ONE
expectation rather than the one line that was reported, so a kind that stopped
being re-entered names its own line.
-}
carets : List String -> List String
carets all =
    let
        lines =
            Array.fromList all
    in
    List.map
        (\i ->
            let
                reg =
                    Scan.regionAt lines 1 (List.length all) i
            in
            String.fromInt i
                ++ " "
                ++ regionWord reg.kind
                ++ " "
                ++ String.fromInt reg.from
                ++ "-"
                ++ String.fromInt reg.to
                ++ " «"
                ++ Scan.markerFor lines reg
                ++ "»"
        )
        (List.range 1 (List.length all - 1))


{-| EVERY CARET A READER CAN ACTUALLY PUT DOWN: every stop the pane draws, and
every line of that stop. What each one writes is the box SEEDED with its region's
own marker plus a word, which is what the shell hands back.
-}
everyCaret : List String -> List String
everyCaret lines =
    let
        m =
            model lines
    in
    List.concatMap
        (\r ->
            List.map
                (\o -> Scan.bodyText (insertedAt r.id (Just o) "NEW" lines) [])
                (List.range 0 (r.to - r.from - 1))
        )
        (List.filter (\r -> r.kind == Para) m.rows)


{-| The writes that SPLIT A TABLE: a body spelling more table runs than the one
it was written into.
-}
splits : List String -> List String
splits lines =
    List.filter (\b -> tableRuns b > tableRuns (String.join "\n" lines)) (everyCaret lines)


{-| The MARKER the row \`+' draws wears, over the stop's line CARET — what the
shell seeds its box with, read off the draw the way the page reads it.
-}
leadAt : String -> Maybe Int -> List String -> String
leadAt id caret lines =
    List.foldr
        (\r acc ->
            if r.id == Scan.draftId then
                r.text

            else
                acc
        )
        ""
        (Maybe.withDefault [] (Scan.drafted (model lines) id caret))


{-| The same, with a paragraph spelling WRITTEN joined under ROW — what \`+' does
before a flush, a key pressed with no box open naming no caret. The rows come
back unchanged where the stop takes none.
-}
inserted : String -> String -> List String -> { rows : List Row, lines : List String }
inserted id written lines =
    insertedAt id Nothing written lines


{-| And with the box opened over the stop's line CARET, which is where `S-RET'
opens one.

WHAT THE SHELL DOES: `+' draws the row and SEEDS THE BOX with its marker, the
reader types after it, and the whole line goes back. So WRITTEN is what the
READER typed and the marker is taken from the draw, which is where the page takes
it from too.

-}
insertedAt :
    String
    -> Maybe Int
    -> String
    -> List String
    -> { rows : List Row, lines : List String }
insertedAt id caret written lines =
    let
        m =
            model lines
    in
    { m
        | rows =
            Maybe.withDefault m.rows
                (Scan.insertion m id caret (leadAt id caret lines ++ written))
    }


{-| THE BOX HOLDS THE MARKER AND THE READER EDITS IT, so a write carries one
WHOLE line. `insertedAt' is the case where they typed AFTER the marker; this is
the case where the marker itself is what they changed — a table row, a source
line, a clock entry.
-}
typedAt :
    String
    -> Maybe Int
    -> String
    -> List String
    -> { rows : List Row, lines : List String }
typedAt id caret written lines =
    let
        m =
            model lines
    in
    { m | rows = Maybe.withDefault m.rows (Scan.insertion m id caret written) }


{-| The draft as it is DRAWN: the line it takes and the rung it hangs off.
-}
drawnAt : String -> Maybe Int -> List String -> Maybe ( Int, Maybe String )
drawnAt id caret lines =
    Scan.drafted (model lines) id caret
        |> Maybe.andThen (List.filter (\r -> r.id == Scan.draftId) >> List.head)
        |> Maybe.map (\r -> ( r.from, r.owner ))


{-| And the line the WRITE put it on, found in the body that write composed.
-}
wroteAt : String -> Maybe Int -> String -> List String -> Maybe Int
wroteAt id caret written lines =
    Scan.bodyText (insertedAt id caret written lines) []
        |> String.split "\n"
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, line ) -> String.contains written line)
        |> List.head
        |> Maybe.map Tuple.first


{-| A BODY WITH A CHILD IN IT, whose bytes are outside this window: two lines of
the entry's own and a `** kid` under them, which no gesture here may reach.
-}
withKid : { rows : List Row, lines : List String }
withKid =
    { rows = Scan.rowsFrom [ "* head", "mine", "** kid" ] 2 [] [ ( 0, 2, [] ) ]
    , lines = [ "* head", "mine", "** kid" ]
    }


{-| A REAL DOCUMENT, off the corpus: the lens's own `body` for one entry, blank
last line and all. TWO top-level items, each carrying a nested run, and one of
those carrying a third level — where every other fixture here has one.
-}
chores : List String
chores =
    [ "* STARTED [#A] Дела по дому :pets:task:"
    , "- [ ] Пёсики"
    , "  - [X] Сухой корм"
    , "  - [X] Симпарика"
    , "  - [X] Мягкий корм"
    , "  - [X] Для зубов"
    , "    - [X] Зубная паста"
    , "    - [X] Зубные щётки Эрику и Юмику"
    , "  - [X] Записать к грумеру"
    , "  - [ ] Постричь когти"
    , "- [ ] Квартира"
    , "  - [ ] Свет"
    , "  - [ ] Уборка"
    , "  - [ ] Плинтусы"
    , "  - [ ] Покраска стен"
    , "  - [ ] Плесень на балконе"
    , ""
    ]


{-| That body with LINES put in at I: an expectation stating what MOVED, so
every byte the splice left alone is asserted rather than restated.
-}
spliced : Int -> List String -> String
spliced i lines =
    String.join "\n" (List.take i chores ++ lines ++ List.drop i chores)


{-| THE CASE AS IT WAS REPORTED: a nested run inside ONE stop, where `S-RET' from
a box over the whole item used to land the new item past everything.
-}
pets : List String
pets =
    [ "* head"
    , "- [ ] Пёсики"
    , "  - [X] Сухой корм"
    , "  - [X] Симпарика"
    , "  - [X] Мягкий корм"
    , "  - [X] Для зубов"
    , "    - [X] Зубная паста"
    , "    - [X] Зубные щётки Эрику и Юмику"
    , "  - [X] Записать к грумеру"
    , "  - [ ] Постричь когти"
    ]


{-| ONE BODY WEARING ALL FIVE KINDS, in the order the region walk meets them.
-}
kinds : List String
kinds =
    [ "* head"
    , "prose"
    , "- item"
    , "| a |"
    , "#+begin_src sh"
    , "echo"
    , "#+end_src"
    , ":LOGBOOK:"
    , "clocked"
    , ":END:"
    ]


srcBlock : List String
srcBlock =
    [ "* head", "#+begin_src sh", "echo one", "echo two", "#+end_src" ]


logbook : List String
logbook =
    [ "* head", "notes", ":LOGBOOK:", "CLOCK: [2026-08-12 Wed 10:00]", ":END:" ]


{-| The proposal's own table, alignment and rule row and all.
-}
grid : List String
grid =
    [ "* head", "| alpha | beta  |", "|-------+-------|", "| one   | two   |" ]


{-| A TABLE NOBODY ALIGNED: two rows spelling two widths per column, so a new
row is padded to the WIDER of each. An aligned fixture says nothing about which
of the two the rule takes.
-}
ragged : List String
ragged =
    [ "* head", "| a | bb |", "| ccc | d |" ]


{-| THE SPEC'S OWN MOTIVATING EXAMPLE: a `#+begin_src` run riding inside a list
item, with a line of the item's under it so the block's closer is not also the
item's last.
-}
itemSrc : List String
itemSrc =
    [ "* head"
    , "- alpha"
    , "  #+begin_src sh"
    , "  echo hi"
    , "  echo bye"
    , "  #+end_src"
    , "  tail"
    , "- beta"
    ]


{-| The same nesting one kind over: a table inside an item.
-}
itemGrid : List String
itemGrid =
    [ "* head", "- alpha", "  | a | bb |", "  | c | d  |", "- beta" ]


{-| And a drawer inside one, `:LOGBOOK:` being the drawer a subtree carries.
-}
itemBook : List String
itemBook =
    [ "* head"
    , "- alpha"
    , "  :LOGBOOK:"
    , "  CLOCK: [2026-08-12 Wed 10:00]"
    , "  :END:"
    , "- beta"
    ]


{-| THE REPORTED BUG'S OWN BODY: a bullet inside a `#+begin_src` inside a list
item. The scanner hunted an item's raw lines for openers knowing nothing of
blocks, so this one was minted as a STOP whose span ran to the block's own
closer — `d` then `D` over it took the `#+end_src` out and left the block open.
-}
itemSrcBullet : List String
itemSrcBullet =
    [ "* head", "- a", "  #+begin_src sh", "  - not an item", "  #+end_src", "- b" ]


{-| THE DRAWER TWIN, AND IT IS NOT THE SAME. A drawer is a GREATER element, so
its contents ARE elements and this bullet IS an item — which is how the corpus's
own `:LOGBOOK:` state lines read. Only the STOPS match the block twin: a region
nested inside an item mints none.
-}
itemBookItem : List String
itemBookItem =
    [ "* head", "- a", "  :LOGBOOK:", "  - an item", "  :END:", "- b" ]


{-| A PIPE ROW INSIDE A VERBATIM BLOCK IS SOURCE. Org reads a table by its own
grammar and `src` suspends it, so the row is a line of the block wherever it
rides. `quoteGrid` below is the same pairing over a block where a pipe row CAN be
a table, which is where the rule can fail.
-}
itemSrcPipes : List String
itemSrcPipes =
    [ "* head", "- a", "  #+begin_src org", "  | x | y |", "  #+end_src", "- b" ]


srcPipes : List String
srcPipes =
    [ "* head", "#+begin_src org", "| a | b |", "#+end_src" ]


{-| A BLOCK INSIDE A DRAWER. The drawer takes the run whole as a STOP question,
and the walk re-enters it, so the block's own lines are the BLOCK's.
-}
itemDrawerBlock : List String
itemDrawerBlock =
    [ "* head"
    , "- a"
    , "  :LOGBOOK:"
    , "  #+begin_src sh"
    , "  echo"
    , "  #+end_src"
    , "  :END:"
    , "- b"
    ]


drawerBlock : List String
drawerBlock =
    [ "* head", ":LOGBOOK:", "#+begin_src sh", "echo", "#+end_src", ":END:" ]



-- ORG'S GREATER/LESSER SPLIT, ASKED OVER EVERY PAIRING
--
-- A GREATER region contains elements and the walk re-enters it; a LESSER one
-- holds none and is opaque.  So every container is paired here with every kind
-- it can hold, at TOP LEVEL and inside an ITEM, and each is asked both ways: the
-- stops it yields, and what every caret in it writes.


{-| THE CORPUS'S OWN SHAPE, off
`~/sync/views/.org-glance/data/gy/m-25044-…/data.org` — a `#+begin_pin`, a tag's
own SPECIAL block, holding a list and then a table. The lines are that file's,
with the schedule and the table cut short and the closer pulled up. A caret
anywhere in the table used to be answered with the BLOCK's empty line, which org
read as `[1 table, 21 rows] -> [2 tables, 21 rows]`.
-}
pinned : List String
pinned =
    [ "* DONE [#A] Cali Exercise 1D, Week 4, Day 22 :gym:ARCHIVE:"
    , "#+begin_pin"
    , "- Schedule:"
    , "  - [2022-01-05 Wed 19:00 ++1w]"
    , ""
    , "| Phase 1 - Week 4  |                    |"
    , "|-------------------+--------------------|"
    , "| Day               | Exercise           |"
    , "| Day 22  Monday    | Pike Pushup        |"
    , "#+end_pin"
    ]


{-| A GREATER BLOCK holding each kind in turn. `quote` is org's own; a block org
does not name is a SPECIAL block and greater the same way.
-}
quoteGrid : List String
quoteGrid =
    [ "* head", "#+begin_quote", "| a | bb |", "| c | d  |", "#+end_quote" ]


quoteList : List String
quoteList =
    [ "* head", "#+begin_quote", "- a", "- b", "#+end_quote" ]


quoteBook : List String
quoteBook =
    [ "* head", "#+begin_quote", ":LOGBOOK:", "clocked", ":END:", "#+end_quote" ]


quoteSrc : List String
quoteSrc =
    [ "* head", "#+begin_quote", "#+begin_src sh", "- echo", "#+end_src", "#+end_quote" ]


{-| A DRAWER holding a table, and one holding a list. `:RESULTS:` because a
drawer is a drawer whatever it is called.
-}
bookGrid : List String
bookGrid =
    [ "* head", ":LOGBOOK:", "| a | bb |", "| c | d  |", ":END:" ]


bookList : List String
bookList =
    [ "* head", ":RESULTS:", "- a", "- b", ":END:" ]


{-| The nested twins: the same two containers riding inside a list item.
-}
itemQuoteGrid : List String
itemQuoteGrid =
    [ "* head"
    , "- alpha"
    , "  #+begin_quote"
    , "  | a | bb |"
    , "  | c | d  |"
    , "  #+end_quote"
    , "- beta"
    ]


itemBookGrid : List String
itemBookGrid =
    [ "* head"
    , "- alpha"
    , "  :LOGBOOK:"
    , "  | a | bb |"
    , "  | c | d  |"
    , "  :END:"
    , "- beta"
    ]


{-| THE FIVE VERBATIM BLOCKS — the names `org-element-greater-elements` leaves
out — each holding a bullet and a pipe row. Org parses no element inside one, so
both are lines of the block, and one name dropped from that list makes its
fixture's bullet an item. `comment` is the name org's LIST rule
(`org-list-forbidden-blocks`, four names) spares and the element rule does not.
-}
verbatims : List ( String, List String )
verbatims =
    List.map (\name -> ( name, blockAround name ))
        [ "comment", "example", "export", "src", "verse" ]


{-| THE OTHER POLARITY, over the same body: org's own two greater blocks and a
tree's own special one, where the bullet IS an item. Membership is a list, so
both sides of it are asked.
-}
greaters : List ( String, List String )
greaters =
    List.map (\name -> ( name, blockAround name )) [ "center", "quote", "pin" ]


blockAround : String -> List String
blockAround name =
    [ "* head", "#+begin_" ++ name, "- a", "| x | y |", "#+end_" ++ name ]


{-| A BLOCK STRADDLING AN ITEM BOUNDARY. Org reads ONE `#+begin_src` here and one
item over all of it; hunting bullets THROUGH the block cut the item at `- b`, and
taking that item carried the opener off without its closer.
-}
straddleSrc : List String
straddleSrc =
    [ "* head", "- a", "  #+begin_src sh", "  echo", "- b", "  #+end_src", "- c" ]


{-| The drawer twin, which org's own `org-list-struct` steps over the same way.
-}
straddleBook : List String
straddleBook =
    [ "* head", "- a", "  :LOGBOOK:", "  clocked", "- b", "  :END:", "- c" ]


{-| AND THE TOP LEVEL IS ITS OWN PATH — nothing there asks the walk, so a bullet
inside a block is the block's because the block was already taken whole.
-}
topBlockBullet : List String
topBlockBullet =
    [ "* head", "#+begin_src sh", "- not an item", "echo", "#+end_src", "after" ]


{-| ONE BLANK LINE STAYS IN, org's rule, and here it stays in a NESTED run: the
line belongs to the item above it and to no stop.
-}
gapRun : List String
gapRun =
    [ "* head", "- a", "  - x", "", "  - y", "- b" ]


{-| A DEEPLY-INDENTED ITEM WITH A WRAPPED CONTINUATION. Only the two column-1
bullets open an item; `beta`'s last two lines are prose of its own.
-}
wrapRun : List String
wrapRun =
    [ "* head"
    , "- alpha"
    , "  - beta"
    , "    wrapped over"
    , "    two more lines"
    , "- gamma"
    ]


{-| EVERY BODY WHOSE WRITES ORG MUST STILL READ — every pairing this suite
carries.
-}
sweep : List (List String)
sweep =
    [ itemSrcBullet
    , itemBookItem
    , itemSrcPipes
    , itemDrawerBlock
    , srcPipes
    , topBlockBullet
    , kinds
    , pinned
    , quoteGrid
    , quoteList
    , quoteBook
    , quoteSrc
    , bookGrid
    , bookList
    , itemQuoteGrid
    , itemBookGrid
    , straddleSrc
    , straddleBook
    , wrapRun
    ]
        ++ List.map Tuple.second verbatims
        ++ List.map Tuple.second greaters


{-| And the ones whose TAKES it must read too. A TOP-LEVEL DRAWER WITH STRUCTURE
IN IT is out, `drawerBlock` for the same reason: a drawer is no stop up there, so
its opener and its closer land in separate paragraph stops and taking one leaves
the other standing. That asymmetry is open, and its own cases pin it.
-}
takeable : List (List String)
takeable =
    List.filter (\body -> not (List.member body [ bookGrid, bookList ])) sweep


suite : Test
suite =
    describe "Scan"
        [ describe "listOpener — the corpus's openers, and the one that is not"
            [ test "a dash opens an item" <|
                \_ -> Expect.equal (Just 0) (indentOf "- alpha")
            , test "so does a plus, and a number under either punctuation" <|
                \_ ->
                    Expect.equal [ Just 0, Just 0, Just 0 ]
                        [ indentOf "+ alpha", indentOf "1. alpha", indentOf "1) alpha" ]
            , test "the indent is the item's, and is what nesting is read off" <|
                \_ -> Expect.equal (Just 4) (indentOf "    - deep")
            , test "an INDENTED star is an item" <|
                \_ -> Expect.equal (Just 2) (indentOf "  * alpha")
            , test "and a star at COLUMN 1 is a headline, which is the whole guard" <|
                \_ -> Expect.equal Nothing (indentOf "* alpha")
            , test "a bullet needs its space: `-word' is prose" <|
                \_ -> Expect.equal Nothing (indentOf "-word")
            , test "and so does a number: `1.5' is not an item" <|
                \_ -> Expect.equal Nothing (indentOf "1.5 apples")
            , test "a bare bullet at the line's end still opens one" <|
                \_ -> Expect.equal (Just 0) (indentOf "-")
            ]
        , describe "listRun — ONE BLANK LINE STAYS IN, org's own rule"
            [ test "one blank between items keeps the list whole" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 4, "composite:list" ), ( 1, 2, "leaf" ), ( 3, 4, "leaf" ) ]
                        (scan [ "* head", "- alpha", "", "- beta" ])
            , test "two blanks close it, and what follows is its own paragraph" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 2, "composite:list" ), ( 1, 2, "leaf" ), ( 4, 5, "element" ) ]
                        (scan [ "* head", "- alpha", "", "", "after" ])
            , test "one blank closes it too when what follows does not ride" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 2, "composite:list" ), ( 1, 2, "leaf" ), ( 3, 4, "element" ) ]
                        (scan [ "* head", "- alpha", "", "after" ])
            , test "an indented continuation rides inside the item above it" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 3, "composite:list" ), ( 1, 3, "leaf" ) ]
                        (scan [ "* head", "- alpha", "  more of alpha" ])
            ]
        , describe "blocksIn — the kinds, and what each is cut at"
            [ test "a paragraph run ends at the blank line under it" <|
                \_ ->
                    Expect.equal [ ( 1, 3, "element" ), ( 4, 5, "element" ) ]
                        (scan [ "* head", "one", "two", "", "three" ])
            , test "a block is ANY #+begin_X with a matching #+end_X" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 4, "composite:quote" ), ( 2, 3, "leaf" ) ]
                        (scan [ "* head", "#+begin_quote", "said so", "#+end_quote" ])
            , test "and it closes by NAME: a mismatched end is not a closer" <|
                \_ ->
                    Expect.equal [ ( 1, 4, "element" ) ]
                        (scan [ "* head", "#+begin_quote", "said so", "#+end_src" ])
            , test "the opener and closer are case-insensitive, as org writes them" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 4, "composite:src" ), ( 2, 3, "leaf" ) ]
                        (scan [ "* head", "#+BEGIN_SRC", "code", "#+End_Src" ])
            , test "a table is a composite whose leaf is a LINE — rules included" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 4, "composite:table" )
                        , ( 1, 2, "leaf" )
                        , ( 2, 3, "leaf" )
                        , ( 3, 4, "leaf" )
                        ]
                        (scan [ "* head", "| a | b |", "|---+---|", "| 1 | 2 |" ])
            , test "an unclosed block is ordinary text" <|
                \_ ->
                    Expect.equal [ ( 1, 3, "element" ) ]
                        (scan [ "* head", "#+begin_quote", "never closed" ])
            , test "OWN cuts the body where the outline under it begins" <|
                \_ ->
                    Expect.equal [ ( 1, 2, "element" ) ]
                        (scanOwn 2 [ "* head", "mine", "** child", "not mine" ])
            ]
        , describe "the grain LADDER — `up' is the IMMEDIATE owner"
            [ test "a flat list's items are owned by the list" <|
                \_ ->
                    Expect.equal [ Nothing, Just 0, Just 0 ]
                        (owners [ "* head", "- alpha", "- beta" ])
            , test "a nested item is owned by the ITEM above it, not by the list" <|
                \_ ->
                    Expect.equal [ Nothing, Just 0, Just 1 ]
                        (owners [ "* head", "- alpha", "  - deeper" ])
            , test "and a third level is owned by the second" <|
                \_ ->
                    Expect.equal [ Nothing, Just 0, Just 1, Just 2 ]
                        (owners [ "* head", "- a", "  - b", "    - c" ])
            , test "a block's runs are owned by the block" <|
                \_ ->
                    Expect.equal [ Nothing, Just 0, Just 0 ]
                        (owners [ "* head", "#+begin_quote", "one", "", "two", "#+end_quote" ])
            ]
        , describe "bodyText — the splice"
            [ test "a model nobody touched gives the body back verbatim" <|
                \_ ->
                    Expect.equal "* head\nalpha\n\nbeta"
                        (Scan.bodyText (model [ "* head", "alpha", "", "beta" ]) [])
            , test "one changed paragraph replaces its lines and nothing else" <|
                \_ ->
                    Expect.equal "* head\nrewritten\n\nbeta"
                        (Scan.bodyText (edited "B0" "rewritten" [ "* head", "alpha", "", "beta" ]) [])
            , test "a replacement of several lines splices them all in" <|
                \_ ->
                    Expect.equal "* head\none\ntwo\n\nbeta"
                        (Scan.bodyText (edited "B0" "one\ntwo" [ "* head", "alpha", "", "beta" ]) [])
            , test "a deletion takes its lines, and the blank under them" <|
                \_ ->
                    Expect.equal "* head\nbeta"
                        (Scan.bodyText (model [ "* head", "alpha", "", "beta" ]) [ "B0" ])
            , test "the LAST paragraph leaves the blank above it standing" <|
                \_ ->
                    Expect.equal "* head\nalpha\n"
                        (Scan.bodyText (model [ "* head", "alpha", "", "beta" ]) [ "B1" ])
            , test "two edits both land, and neither moves the other's range" <|
                \_ ->
                    let
                        m =
                            edited "B0" "first" [ "* head", "alpha", "", "beta" ]

                        both =
                            { m
                                | rows =
                                    List.map
                                        (\r ->
                                            if r.id == "B1" then
                                                { r | text = "second" }

                                            else
                                                r
                                        )
                                        m.rows
                            }
                    in
                    Expect.equal "* head\nfirst\n\nsecond" (Scan.bodyText both [])
            ]
        , describe "ONE GRAIN SPEAKS FOR A RANGE"
            [ test "a moved composite silences the leaves under it" <|
                \_ ->
                    -- The list is B0 and its items B1 and B2 over the same lines.
                    -- Rewriting the whole list must not then splice the items in
                    -- on top of it.
                    Expect.equal "* head\n- rewritten whole"
                        (Scan.bodyText
                            (edited "B0" "- rewritten whole" [ "* head", "- alpha", "- beta" ])
                            []
                        )
            , test "a GOING composite silences them, alone or flagged with one" <|
                \_ ->
                    -- The pair's real occasion is the second: without the
                    -- silencing the item would splice at a range the list's own
                    -- deletion has already taken out.
                    Expect.equal [ "* head", "* head" ]
                        [ Scan.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0" ]
                        , Scan.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0", "B1" ]
                        ]
            , test "an edited item under an edited list is the LIST's text" <|
                \_ ->
                    -- The rule's real occasion is the GROWING edit.  Bottom-up
                    -- ordering alone keeps most of these right, because the
                    -- list's range covers the item's and lands last; what it
                    -- cannot survive is a leaf splice that CHANGES THE LINE
                    -- COUNT under it.
                    let
                        under written =
                            let
                                m =
                                    edited "B0" "- whole" [ "* head", "- alpha", "- beta" ]
                            in
                            Scan.bodyText
                                { m
                                    | rows =
                                        List.map
                                            (\r ->
                                                if r.id == "B1" then
                                                    { r | text = written }

                                                else
                                                    r
                                            )
                                            m.rows
                                }
                                []
                    in
                    Expect.equal [ "* head\n- whole", "* head\n- whole" ]
                        [ under "- item\n- extra", under "- item" ]
            , test "an untouched composite lets its own leaf speak" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- rewritten"
                        (Scan.bodyText
                            (edited "B2" "- rewritten" [ "* head", "- alpha", "- beta" ])
                            []
                        )
            ]
        , describe "insertion — where a paragraph joins"
            [ test "under the paragraph at point, one blank between" <|
                \_ ->
                    Expect.equal "* head\nalpha\n\nmid\n\nbeta"
                        (Scan.bodyText (inserted "B0" "mid" [ "* head", "alpha", "", "beta" ]) [])

            -- THE BLANK BELOW IS DECIDED, never spelled: prose at the carrier's
            -- end reads back as ONE paragraph with what was written, so the
            -- separator is asked of the line rather than fixed at "\n\n".
            , test "and a blank below where what follows is prose" <|
                \_ ->
                    Expect.equal "* head\n- a\n\nnote\n\nafter"
                        (Scan.bodyText (inserted "B0" "note" [ "* head", "- a", "after" ]) [])
            , test "the headline's leads the body" <|
                \_ ->
                    Expect.equal "* head\ntop\n\nalpha\n\nbeta"
                        (Scan.bodyText (inserted "H" "top" [ "* head", "alpha", "", "beta" ]) [])
            , test "and a body with no block at all is seeded with one" <|
                \_ ->
                    Expect.equal "* head\nfirst\n"
                        (Scan.bodyText (inserted "H" "first" [ "* head", "" ]) [])

            -- AN ITEM JOINS STRICTLY BELOW THE STOP, wearing the stop's own
            -- prefix.  The typed text is what the READER typed, so the lead
            -- appears in the expectation and never in the argument.  ORG'S OWN
            -- `M-RET': the reader walked to an item and the new one belongs
            -- under THAT one, never at a bottom they would walk back up from, so
            -- the run below it stays where it is however long.
            , test "an item's joins STRICTLY BELOW the stop, the run untouched" <|
                \_ ->
                    Expect.equal
                        [ "* head\n- alpha\n- note\n- beta"
                        , "* head\n- alpha\n- note\n- beta\n- gamma"
                        ]
                        [ Scan.bodyText
                            (inserted "B1" "note" [ "* head", "- alpha", "- beta" ])
                            []
                        , Scan.bodyText
                            (inserted "B1" "note" [ "* head", "- alpha", "- beta", "- gamma" ])
                            []
                        ]

            -- ONE BLANK STAYS INSIDE THE RUN (org's rule, `listRun'), and a
            -- sibling of the stop goes above it, the blank belonging to what
            -- follows rather than to the item being joined.
            , test "a blank line inside the run stays under the new item" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- note\n\n- beta"
                        (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "", "- beta" ]) [])

            -- THE INDENT IS THE CURSOR'S: the nested run's own bottom, two
            -- spaces in.
            , test "a nested item's joins the NESTED run, at the stop's indent" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - deep\n  - note\n- beta"
                        (Scan.bodyText
                            (inserted "B2" "note" [ "* head", "- alpha", "  - deep", "- beta" ])
                            []
                        )

            -- AND AN OUTER ITEM'S BOTTOM IS PAST ITS OWN NESTED RUN, `joined'
            -- walking everything the last sibling owns.
            , test "an item carrying a nested run keeps it above the new sibling" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - deep\n- note"
                        (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "  - deep" ]) [])

            -- A NUMBER CONTINUES OFF THE LAST ITEM: the stop's own number
            -- spelled at the bottom is a duplicate, which is what makes org
            -- renumber.
            -- THE NUMBER IS THE STOP'S, ONE ON, and the item below keeps the
            -- one it had — a duplicate, which is org's own `M-RET' answer and
            -- what `org-list-repair' is for.  Counting from the run's bottom
            -- would spell a number two items away from where this lands.
            , test "a numbered item takes the stop's number, one on" <|
                \_ ->
                    Expect.equal "* head\n1. alpha\n2. note\n2. beta"
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

            -- A TABLE LINE AND A BLOCK RUN KEEP THE COMPOSITE'S LANDING WITH NO
            -- CARET, and the reason is the GRAIN: `+' with no box open names no
            -- line, so there is nothing to be inside and the answer is a sibling
            -- of the STOP.  A caret is what makes a region's interior
            -- addressable, and the cases above are where it does.  Each is asked
            -- twice, since the blank BELOW is decided rather than spelled: with
            -- prose after the structure, and with the structure ending the body.
            , test "a table's line rides the table, which stays whole" <|
                \_ ->
                    Expect.equal
                        [ "* head\n| a |\n| b |\n\nnote\n\nafter"
                        , "* head\n| a |\n| b |\n\nnote"
                        ]
                        [ Scan.bodyText
                            (inserted "B2" "note" [ "* head", "| a |", "| b |", "", "after" ])
                            []
                        , Scan.bodyText
                            (inserted "B1" "note" [ "* head", "| a |", "| b |" ])
                            []
                        ]
            , test "a block's run rides the block, so no prose lands in source" <|
                \_ ->
                    Expect.equal
                        [ "* head\n#+begin_src\nx\n#+end_src\n\nnote\n\nafter"
                        , "* head\n#+begin_src sh\necho one\n#+end_src\n\nnote"
                        ]
                        [ Scan.bodyText
                            (inserted "B1"
                                "note"
                                [ "* head", "#+begin_src", "x", "#+end_src", "", "after" ]
                            )
                            []
                        , Scan.bodyText
                            (inserted "B1"
                                "note"
                                [ "* head", "#+begin_src sh", "echo one", "#+end_src" ]
                            )
                            []
                        ]
            , test "a child takes none, its bytes being outside this window" <|
                \_ ->
                    Expect.equal Nothing (Scan.insertion withKid "C0" Nothing "note")
            , test "nor an id no row wears" <|
                \_ ->
                    Expect.equal Nothing
                        (Scan.insertion (model [ "* head", "alpha" ]) "B9" Nothing "note")
            ]

        -- `+' DRAWS THE ROW BEFORE ANYTHING IS WRITTEN.  It is zero-width and
        -- empty, so its text has not moved off its `was' and the splice passes
        -- it over: the reader sees the line they are about to fill and the file
        -- is the file it was.
        , describe "drafted — a paragraph drawn before it is written"
            [ test "the drawn row writes nothing at all" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha", "", "beta" ]

                        rows =
                            Maybe.withDefault m.rows (Scan.drafted m "B0" Nothing)
                    in
                    Expect.equal ( 4, "* head\nalpha\n\nbeta" )
                        ( List.length rows, Scan.bodyText { m | rows = rows } [] )
            , test "the drawn item writes nothing at all either" <|
                \_ ->
                    -- Its text IS its `was', which is what makes the lead free.
                    let
                        m =
                            model [ "* head", "- alpha" ]

                        rows =
                            Maybe.withDefault m.rows (Scan.drafted m "B1" Nothing)
                    in
                    Expect.equal ( 4, "* head\n- alpha" )
                        ( List.length rows, Scan.bodyText { m | rows = rows } [] )
            , test "and it stands directly under the stop, inside the list" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "- alpha", "- beta", "", "after" ]
                    in
                    Expect.equal (Just [ "H", "B0", "B1", "D", "B2", "B3" ])
                        (Maybe.map (List.map .id) (Scan.drafted m "B1" Nothing))
            , test "a second ask draws one paragraph rather than two" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        once =
                            Maybe.withDefault m.rows (Scan.drafted m "B0" Nothing)

                        twice =
                            Maybe.withDefault once (Scan.drafted { m | rows = once } "B0" Nothing)
                    in
                    Expect.equal 1
                        (List.length (List.filter (\r -> r.id == Scan.draftId) twice))
            , test "and undrafted leaves behind what it found" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        rows =
                            Maybe.withDefault m.rows (Scan.drafted m "B0" Nothing)
                    in
                    Expect.equal (List.map .id m.rows)
                        (List.map .id (Scan.undrafted { m | rows = rows }))
            ]

        -- WHERE THE CURSOR IS OWED after the write: block ids are POSITIONAL,
        -- so the row an insert makes has no id until the rescan mints one and
        -- the LINE it starts at is what names it instead.
        -- TWO FAULTS THE TEXT SUITE COULD NOT SEE, both found by driving a real
        -- browser over a nested run.  Each is stated here as the rule it broke.
        , describe "a drafted item is a leaf of the run it joins"
            [ test "it owns what its siblings own, so the composite still draws it" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "- alpha", "  - nested", "- beta" ]

                        drawn =
                            Maybe.withDefault [] (Scan.drafted m "B2" Nothing)

                        held =
                            List.map (\r -> ( r.id, r.owner ))
                                (List.filter (\r -> r.id == Scan.draftId) drawn)
                    in
                    -- `Doc.viewKids' walks a composite's kids while their owner
                    -- is its own; a draft owning NOBODY breaks that walk and the
                    -- leaves past it are drawn a second time as the gap text.
                    Expect.equal [ ( "D", Just "B1" ) ] held
            , test "and a multi-line item rides inside itself" <|
                \_ ->
                    -- A continuation at column 1 closes the run: org reads it as
                    -- a paragraph, so the reader's ONE item became two things.
                    Expect.equal "* head\n- alpha\n- one\n  two"
                        (Scan.bodyText
                            (inserted "B1" "one\ntwo" [ "* head", "- alpha" ])
                            []
                        )
            , test "where a paragraph keeps its blank lines and no indent" <|
                \_ ->
                    Expect.equal "* head\npara\n\none\ntwo"
                        (Scan.bodyText
                            (inserted "B0" "one\ntwo" [ "* head", "para" ])
                            []
                        )
            ]
        -- THE CARET PICKS THE LINE THE LEAD IS SPELLED OFF.  A stop can hold
        -- several lines, and `S-RET' in a box over one hands the next stop the
        -- prefix of the line the press was made on rather than the prefix of
        -- the line the stop opens with.  The shell sends a NUMBER and nothing
        -- else: the grammar is all on this side.
        , describe "the caret's line — which line a lead is spelled off"
            [ test "a deeper line yields its own indent and its own bullet" <|
                \_ ->
                    Expect.equal "  + "
                        (leadAt "B1" (Just 1) [ "* head", "- alpha", "  + beta", "- gamma" ])
            -- AN ABSENT INDEX IS `+' PRESSED WITH NO BOX OPEN, so there is no
            -- caret to read and the stop's first line spells the lead.
            , test "and an absent one stays the stop's first" <|
                \_ ->
                    Expect.equal "- "
                        (leadAt "B1" Nothing [ "* head", "- alpha", "  + beta", "- gamma" ])
            , test "a numbered line continues off ITS number, not the stop's" <|
                \_ ->
                    Expect.equal ( "   4. ", "2. " )
                        ( leadAt "B1" (Just 1) [ "* head", "1. one", "   3. three", "2. two" ]
                        , leadAt "B1" Nothing [ "* head", "1. one", "   3. three", "2. two" ]
                        )
            , test "an index past the stop's last line clamps to it" <|
                \_ ->
                    Expect.equal "  - "
                        (leadAt "B1" (Just 9) [ "* head", "- alpha", "  - beta" ])

            -- THE INDEX CHANGES WHICH LINE IS CONSULTED, NEVER WHETHER A LEAD
            -- EXISTS.  A run's deeper lines can be continuations rather than
            -- items, and the stop's first line is what spells the prefix there
            -- — without the fallback the whole stop loses its grammar and the
            -- sibling leaves as a paragraph past the list.
            , test "a continuation line spells no item, so the stop's first does" <|
                \_ ->
                    Expect.equal ( "- ", "* head\n- alpha\n  more of alpha\n- note\n- beta" )
                        ( leadAt "B1" (Just 1) [ "* head", "- alpha", "  more of alpha", "- beta" ]
                        , Scan.bodyText
                            (insertedAt "B1"
                                (Just 1)
                                "note"
                                [ "* head", "- alpha", "  more of alpha", "- beta" ]
                            )
                            []
                        )

            -- A PARAGRAPH'S LINES ARE ONE VALUE rather than a sequence of
            -- slots, so its region answers the same wherever the caret stood:
            -- a paragraph of its own, past the prose.
            , test "a paragraph takes none, at any index" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "one", "two" ]
                    in
                    Expect.equal (Scan.drafted m "B0" (Just 0)) (Scan.drafted m "B0" (Just 2))

            -- THE WRITE MEASURES THE SAME LEAD.  `draftRow' indents a
            -- multi-line item's continuations by the lead's own width, so a
            -- draw at one line and a write at another would ride the reader's
            -- second line under a bullet it never wore.
            , test "and the write rides continuations under the bullet drawn" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - beta\n  - one\n    two\n- gamma"
                        (Scan.bodyText
                            (insertedAt "B1"
                                (Just 1)
                                "one\ntwo"
                                [ "* head", "- alpha", "  - beta", "- gamma" ]
                            )
                            []
                        )
            ]

        -- THE CARET'S LINE ANCHORS THE INSERT, and the reported case is the
        -- fixture: a run nested inside ONE stop, a box open over the whole of
        -- it, and `S-RET' pressed on a line in the middle.  The new item used
        -- to land past everything.
        , describe "the caret's line — where the sibling lands"
            [ test "a caret mid-run splits it, and what was below stays below" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n    - [ ] Ошейник\n"
                            ++ "  - [X] Записать к грумеру\n  - [ ] Постричь когти"
                        )
                        (Scan.bodyText (insertedAt "B1" (Just 6) "Ошейник" pets) [])

            -- THE CASE THAT KEEPS THE CHANGE HONEST: at the run's last line the
            -- split and the old bottom-of-the-structure answer are one line, so
            -- the two rules can only be told apart in the middle.
            , test "at the run's LAST line the split is the bottom" <|
                \_ ->
                    Expect.equal ( Just 10, Just 10 )
                        ( Scan.joinLine (model pets) "B1" (Just 8)
                        , Scan.joinLine (model pets) "B1" Nothing
                        )
            , test "and the item it writes there wears that line's own indent" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n  - [X] Записать к грумеру\n"
                            ++ "  - [ ] Постричь когти\n  - [ ] Ошейник"
                        )
                        (Scan.bodyText (insertedAt "B1" (Just 8) "Ошейник" pets) [])

            -- LINE 0 IS A LINE A READER STOOD ON, which is the whole reason an
            -- absent index cannot be spelled as one.
            , test "a caret on line 0 lands under line 0, not past the structure" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n- [ ] Ошейник\n  - [X] Сухой корм\n"
                            ++ "  - [X] Симпарика\n  - [X] Мягкий корм\n  - [X] Для зубов\n"
                            ++ "    - [X] Зубная паста\n    - [X] Зубные щётки Эрику и Юмику\n"
                            ++ "  - [X] Записать к грумеру\n  - [ ] Постричь когти"
                        )
                        (Scan.bodyText (insertedAt "B1" (Just 0) "Ошейник" pets) [])

            -- AND `+' WITH NO BOX OPEN RIDES PAST THE WHOLE STRUCTURE, which is
            -- right where nothing named a line to split.
            , test "an absent index still rides past the whole structure" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n  - [X] Записать к грумеру\n"
                            ++ "  - [ ] Постричь когти\n- [ ] Ошейник"
                        )
                        (Scan.bodyText (inserted "B1" "Ошейник" pets) [])

            -- ONE LINE, TWO READERS.  The draft is DRAWN before it is written
            -- and the write composes the body a second time; a draw at one line
            -- and a splice at another is the fault this pins.
            , test "the drawn draft and the written line are one line" <|
                \_ ->
                    Expect.equal ( Just ( 8, Just "B5" ), Just 8, Just 8 )
                        ( drawnAt "B1" (Just 6) pets
                        , wroteAt "B1" (Just 6) "Ошейник" pets
                        , Scan.joinLine (model pets) "B1" (Just 6)
                        )

            -- AND IT STANDS IN THE ROW ORDER ITS BYTES ARE IN.  `Doc.viewKids'
            -- walks a composite's kids while their owner is its own and reads
            -- the gap off each one's `to', so a draft drawn past the rungs it
            -- was written above has them drawn a SECOND time under it.
            , test "and in the row order its bytes are in" <|
                \_ ->
                    Expect.equal
                        (Just [ "H", "B0", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "D", "B8", "B9" ])
                        (Maybe.map (List.map .id) (Scan.drafted (model pets) "B1" (Just 6)))
            ]
        -- REPORTED AGAINST THE RUNNING APP over the body above: the region "is
        -- not considered as a list", and `S-RET' in it "resolves into empty
        -- row".  The scanner reads it as one list and always did; what answered
        -- with no lead is the stop a reader meets FIRST — one `n' off the
        -- headline is the whole-list composite, and a box opened there covers
        -- every line of it.  A LINE INSIDE A LIST BELONGS TO AN ITEM however
        -- wide the stop laid over it, so the caret is what decides.
        , describe "a real list, and the box laid over the whole of it"
            [ test "the reported body is ONE list, over two top-level runs" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 16, "composite:list" )
                        , ( 1, 10, "leaf" )
                        , ( 2, 3, "leaf" )
                        , ( 3, 4, "leaf" )
                        , ( 4, 5, "leaf" )
                        , ( 5, 8, "leaf" )
                        , ( 6, 7, "leaf" )
                        , ( 7, 8, "leaf" )
                        , ( 8, 9, "leaf" )
                        , ( 9, 10, "leaf" )
                        , ( 10, 16, "leaf" )
                        , ( 11, 12, "leaf" )
                        , ( 12, 13, "leaf" )
                        , ( 13, 14, "leaf" )
                        , ( 14, 15, "leaf" )
                        , ( 15, 16, "leaf" )
                        ]
                        -- 16 is the lens's own `ownLines'; the blank last line
                        -- is outside it and closes the run either way.
                        (scanOwn 16 chores)

            -- A SHALLOWER ITEM UNDER A DEEPER ONE ENDS THE DEEPER RUN EXACTLY,
            -- which is what puts `Записать к грумеру' back under `Пёсики'
            -- rather than under `Для зубов'.
            , test "and the ladder holds, three levels down and back up" <|
                \_ ->
                    Expect.equal
                        [ Nothing, Just 0, Just 1, Just 1, Just 1, Just 1, Just 5, Just 5 ]
                        (List.take 8 (owners chores))
            , test "the second top run is the list's too, and its items are its own" <|
                \_ ->
                    Expect.equal
                        [ Just 1, Just 1, Just 0, Just 10, Just 10, Just 10, Just 10, Just 10 ]
                        (List.drop 8 (owners chores))
            , test "every rung spells its own prefix, at each of the three depths" <|
                \_ ->
                    Expect.equal [ "- [ ] ", "  - [ ] ", "    - [ ] ", "  - [ ] " ]
                        [ leadAt "B1" Nothing chores
                        , leadAt "B2" Nothing chores
                        , leadAt "B6" Nothing chores
                        , leadAt "B11" Nothing chores
                        ]

            -- THE REPORTED SYMPTOM.  The stop is the whole list and the caret
            -- names a line two levels in, so the item wears THAT line's prefix
            -- and joins THAT run — the same answer the leaf under the caret
            -- gives, which is what makes the width of the box stop mattering.
            , test "S-RET in a box over the whole list writes the caret line's own item" <|
                \_ ->
                    Expect.equal ( "    - [ ] ", spliced 8 [ "    - [ ] Ошейник" ] )
                        ( leadAt "B0" (Just 6) chores
                        , Scan.bodyText (insertedAt "B0" (Just 6) "Ошейник" chores) []
                        )
            , test "and the row is drawn in the run it was written into" <|
                \_ ->
                    Expect.equal (Just ( 8, Just "B5" ))
                        (drawnAt "B0" (Just 6) chores)

            -- THE RULE STAYS THE GRAIN'S WHERE NOTHING NAMED A LINE.  `+' with
            -- no box open rides past the whole structure, which is the only way
            -- to put a paragraph after a list — one `b' from any item.
            , test "and `+' with no box open still lands a paragraph past the list" <|
                \_ ->
                    Expect.equal ( "", spliced 16 [ "", "note" ] )
                        ( leadAt "B0" Nothing chores
                        , Scan.bodyText (inserted "B0" "note" chores) []
                        )

            -- AND THE REGION IS ASKED ABOUT, never the line alone.  A source
            -- block can hold a line that OPENS LIKE AN ITEM, and the block's
            -- own answer is what it takes: X's grammar is X's, and a list rule
            -- that accepts more than org does is the worse bug.  The line lands
            -- INSIDE, which is where the reader was standing.
            , test "a block line that looks like an item still spells no bullet" <|
                \_ ->
                    let
                        src =
                            [ "* head", "#+begin_src", "- not an item", "x", "#+end_src", "after" ]
                    in
                    Expect.equal
                        ( ""
                        , "* head\n#+begin_src\n- not an item\nnote\nx\n#+end_src\nafter"
                        )
                        ( leadAt "B1" (Just 0) src
                        , Scan.bodyText (insertedAt "B1" (Just 0) "note" src) []
                        )
            -- A CONTINUATION LINE NAMES NO ITEM, and over a whole-list box the
            -- stop's own first line is the LIST's first item — a column-1
            -- bullet spliced into the middle of a nested run.  The rung HOLDING
            -- the line spells it instead, which is the row `anchored' hangs the
            -- sibling off, so one row answers both.
            , test "a caret on a continuation takes the rung that holds it" <|
                \_ ->
                    let
                        riding =
                            [ "* head", "- alpha", "  - deep", "    inside deep", "- beta" ]
                    in
                    Expect.equal ( "  - ", "* head\n- alpha\n  - deep\n    inside deep\n  - note\n- beta" )
                        ( leadAt "B0" (Just 2) riding
                        , Scan.bodyText (insertedAt "B0" (Just 2) "note" riding) []
                        )
            ]
        -- ONE QUESTION — WHICH REGION HOLDS THE CARET — AND ONE ANSWER PER
        -- REGION.  The reported bug was a caret on a non-item line splicing an
        -- ITEM into the middle of a `#+begin_src' run and cutting the block in
        -- half: the list's rule was the only rule there was, and every other
        -- kind fell through it.
        , describe "the region holding the caret, and what it says a line is"
            [ test "each kind is recognized as itself" <|
                \_ ->
                    Expect.equal
                        [ ( "plain", 1, 2 )
                        , ( "item", 2, 3 )
                        , ( "table", 3, 4 )
                        , ( "block", 4, 7 )
                        , ( "drawer", 7, 10 )
                        ]
                        (List.map (\i -> regionOf i kinds) [ 1, 2, 3, 5, 8 ])
            , test "and each says what a new line inside it opens with" <|
                \_ ->
                    Expect.equal [ "", "- ", "|   |", "", "" ]
                        (List.map (\i -> markerOf i kinds) [ 1, 2, 3, 5, 8 ])

            -- THE FIRST REPORTED BUG.  `- NEW' spliced between two source lines
            -- is a block cut in half, and the box a reader holds over the run
            -- puts their only caret inside it.
            , test "a caret inside a #+begin_src adds an EMPTY line inside it" <|
                \_ ->
                    Expect.equal ( "", Just ( 3, Just "B1" ) )
                        ( leadAt "B1" (Just 0) srcBlock
                        , drawnAt "B1" (Just 0) srcBlock
                        )
            , test "and what is written there lands inside, the block still closing" <|
                \_ ->
                    Expect.equal "* head\n#+begin_src sh\necho one\necho mid\necho two\n#+end_src"
                        (Scan.bodyText (typedAt "B1" (Just 0) "echo mid" srcBlock) [])

            -- A DRAWER IS NO STOP — the scanner leaves it inside the paragraph
            -- it sits in — so recognizing it is what makes this row reachable.
            , test "a drawer takes the same answer, :LOGBOOK: included" <|
                \_ ->
                    Expect.equal
                        ( ""
                        , "* head\nnotes\n:LOGBOOK:\nCLOCK: [2026-08-12 Wed 10:00]\nCLOCK: later\n:END:"
                        )
                        ( leadAt "B0" (Just 2) logbook
                        , Scan.bodyText (typedAt "B0" (Just 2) "CLOCK: later" logbook) []
                        )
            , test "and each wears its OWN indent where it carries one" <|
                \_ ->
                    Expect.equal
                        [ ( ( "drawer", 1, 4 ), "  " )
                        , ( ( "block", 1, 4 ), "  " )
                        , ( ( "table", 1, 3 ), "  |   |   |" )
                        ]
                        [ ( regionOf 2 [ "* head", "  :LOGBOOK:", "  CLOCK: x", "  :END:" ]
                          , markerOf 2 [ "* head", "  :LOGBOOK:", "  CLOCK: x", "  :END:" ]
                          )
                        , ( regionOf 2 [ "* head", "  #+begin_src sh", "  echo", "  #+end_src" ]
                          , markerOf 2 [ "* head", "  #+begin_src sh", "  echo", "  #+end_src" ]
                          )
                        , ( regionOf 1 [ "* head", "  | a | b |", "  | c | d |" ]
                          , markerOf 1 [ "* head", "  | a | b |", "  | c | d |" ]
                          )
                        ]

            -- ORG'S OWN ALIGNMENT, measured off the rows the table already
            -- spells.  A BLANK LINE ENDS A TABLE, so an empty line is no
            -- continuation of one.
            , test "a table's is an EMPTY ROW at the table's own widths" <|
                \_ ->
                    Expect.equal "|       |       |" (markerOf 1 grid)
            , test "and the RULE ROWS are out of the measurement" <|
                \_ ->
                    -- `|-------+-------|' is seven wide in a column of three;
                    -- measuring it would pad every new row to the dashes.
                    Expect.equal "|   |   |"
                        (markerOf 1 [ "* head", "| a | b |", "|-------+-------|", "| c | d |" ])

            -- A RAGGED TABLE is what says WHICH width: org pads to the WIDER,
            -- so every row of the table stays as wide as its widest cell.  An
            -- aligned fixture answers the same under either rule.
            , test "a ragged table pads each column to its WIDEST cell" <|
                \_ ->
                    Expect.equal "|     |    |" (markerOf 1 ragged)
            -- A MARKER IS A LEAD AND POINT GOES AFTER IT — except a table row,
            -- which is a WHOLE row: typing past its closing pipe opens a column
            -- org's own align would then keep, so point goes in the first cell.
            , test "and point lands where the reader types, one space into a row" <|
                \_ ->
                    Expect.equal [ 2, 6, 0, 2, 4, 1 ]
                        (List.map Scan.caretIn
                            [ "- ", "- [ ] ", "", "|   |   |", "  |   |    |", "||" ]
                        )
            , test "the row it writes keeps the table one table" <|
                \_ ->
                    let
                        body =
                            Scan.bodyText (typedAt "B0" (Just 2) "| three | four  |" grid) []
                    in
                    Expect.equal
                        ( "* head\n| alpha | beta  |\n|-------+-------|\n| one   | two   |\n| three | four  |"
                        , [ ( 1, 5, "composite:table" )
                          , ( 1, 2, "leaf" )
                          , ( 2, 3, "leaf" )
                          , ( 3, 4, "leaf" )
                          , ( 4, 5, "leaf" )
                          ]
                        )
                        ( body, scan (String.split "\n" body) )

            -- A CLOSER IS THE REGION'S LAST LINE and a caret on it asks for
            -- what comes AFTER: a line past a closer is outside the region, and
            -- outside every region is prose.  A TABLE HAS NO CLOSER, so its
            -- last row keeps the new row inside — which is how a table is
            -- actually built.
            , test "a caret on #+end_X lands AFTER the block" <|
                \_ ->
                    let
                        src =
                            [ "* head", "#+begin_src sh", "echo one", "#+end_src" ]
                    in
                    Expect.equal ( "", "* head\n#+begin_src sh\necho one\n#+end_src\n\nafter" )
                        ( leadAt "B0" (Just 2) src
                        , Scan.bodyText (typedAt "B0" (Just 2) "after" src) []
                        )
            , test "and one on :END: lands after the drawer" <|
                \_ ->
                    let
                        book =
                            [ "* head", ":LOGBOOK:", "clocked", ":END:" ]
                    in
                    Expect.equal "* head\n:LOGBOOK:\nclocked\n:END:\n\nafter"
                        (Scan.bodyText (typedAt "B0" (Just 2) "after" book) [])
            , test "where a table's last row is a line INSIDE it" <|
                \_ ->
                    -- The table lands at its own bottom, the block a blank line
                    -- past the closer: same index, two regions, two answers.
                    Expect.equal ( Just 4, Just 5 )
                        ( Scan.joinLine (model grid) "B0" (Just 2)
                        , Scan.joinLine
                            (model [ "* head", "#+begin_src sh", "echo one", "#+end_src" ])
                            "B0"
                            (Just 2)
                        )

            -- ORG'S OWN `M-RET': the number continues off the CARET'S line, and
            -- the duplicate below is what `org-list-repair' is for.
            , test "a numbered list continues from the caret's own number" <|
                \_ ->
                    let
                        nums =
                            [ "* head", "1. one", "2. two", "3. three" ]
                    in
                    Expect.equal ( "3. ", "* head\n1. one\n2. two\n3. mid\n3. three" )
                        ( leadAt "B0" (Just 1) nums
                        , Scan.bodyText (insertedAt "B0" (Just 1) "mid" nums) []
                        )
            , test "and a checkbox comes back EMPTY whatever the caret line's state" <|
                \_ ->
                    Expect.equal [ "- [ ] ", "- [ ] ", "- [ ] ", "- [ ] " ]
                        (List.map
                            (\i ->
                                leadAt "B0"
                                    (Just i)
                                    [ "* head", "- [ ] a", "- [X] b", "- [-] c", "- [x] d" ]
                            )
                            [ 0, 1, 2, 3 ]
                        )

            -- AND THE RULE WIDENS NOTHING.  Text org declines is text here too,
            -- and a caret on it takes the PROSE answer.
            , test "a bullet with no space after it is prose" <|
                \_ ->
                    Expect.equal ( ( "plain", 1, 3 ), "" )
                        ( regionOf 1 [ "* head", "-word", "more" ]
                        , markerOf 1 [ "* head", "-word", "more" ]
                        )
            , test "a #+begin_ with no matching #+end_ is prose too" <|
                \_ ->
                    Expect.equal ( ( "plain", 1, 3 ), "" )
                        ( regionOf 2 [ "* head", "#+begin_src", "x" ]
                        , markerOf 2 [ "* head", "#+begin_src", "x" ]
                        )
            , test "a pipe row under a blank line opens a table of its own" <|
                \_ ->
                    -- Out of the region AND out of the measurement: a five-wide
                    -- cell below the blank leaves the widths above it alone.
                    Expect.equal ( ( "table", 1, 2 ), "|   |    |" )
                        ( regionOf 1 [ "* head", "| a | bb |", "", "| ccccc |" ]
                        , markerOf 1 [ "* head", "| a | bb |", "", "| ccccc |" ]
                        )
            , test "and `:a:b:' is no drawer — org's own charset holds" <|
                \_ ->
                    -- EVERY FIXTURE CLOSES, or the line declined for its NAME
                    -- would be declined for the missing `:END:' anyway and the
                    -- charset would be asserting nothing.
                    Expect.equal
                        [ ( "plain", 1, 4 ), ( "plain", 1, 4 ), ( "drawer", 1, 4 ) ]
                        [ regionOf 1 [ "* head", ":a:b:", "x", ":END:" ]
                        , regionOf 1 [ "* head", ":MY DRAWER:", "x", ":END:" ]
                        , regionOf 1 [ "* head", ":ok-name_1:", "x", ":END:" ]
                        ]

            -- ORG CLOSES A DRAWER ON `:end:' TOO, its reader folding the word
            -- where the opener's charset does not.
            , test "and a lowercase :end: closes one" <|
                \_ ->
                    Expect.equal ( "drawer", 1, 4 )
                        (regionOf 2 [ "* head", ":LOGBOOK:", "clocked", ":end:" ])

            -- A CONTINUATION LINE TAKES THE ITEM HOLDING IT, which is the rule
            -- that keeps a column-1 bullet out of a nested run — and it is also
            -- what makes a caret on `beta's wrapped prose answer with `beta's
            -- own bullet, so `S-RET' there cuts the paragraph in two and org
            -- reads a THIRD item.  Today's answer, pinned as it stands:
            -- an adversarial sweep found 612 such writes over about four corpus
            -- documents, and whether that is org's `M-RET' (which does open an
            -- item from a continuation line) or a paragraph split is a decision
            -- nobody has taken.  A change here names it by turning this red.
            , test "a continuation line answers with its item's own bullet" <|
                \_ ->
                    Expect.equal
                        [ "1 item 1-5 «- »"
                        , "2 item 2-5 «  - »"
                        , "3 item 2-5 «  - »"
                        , "4 item 2-5 «  - »"
                        , "5 item 5-6 «- »"
                        ]
                        (carets wrapRun)
            , test "so a caret on the first wrapped line splits the prose" <|
                \_ ->
                    Expect.equal
                        ("* head\n- alpha\n  - beta\n    wrapped over\n  - NEW\n"
                            ++ "    two more lines\n- gamma"
                        )
                        (Scan.bodyText (typedAt "B2" (Just 1) "  - NEW" wrapRun) [])
            ]

        -- AN ITEM'S LINES ARE A BODY OF THEIR OWN, and the walk used to stop at
        -- the item: a caret inside a block riding under `- alpha' was answered
        -- with `- ', which spliced an item into the middle of the block and left
        -- it open.  Every kind gets its nested twin here, since 108 cases tested
        -- each of them at TOP LEVEL alone.
        , describe "a region nested inside an ITEM — the walk re-enters it"
            [ test "a #+begin_src under an item is the BLOCK, not the item" <|
                \_ ->
                    Expect.equal ( ( "block", 2, 6 ), "  " )
                        ( regionOf 3 itemSrc, markerOf 3 itemSrc )
            , test "so the line lands inside it and the block still closes" <|
                \_ ->
                    let
                        body =
                            Scan.bodyText (typedAt "B1" (Just 2) "  echo mid" itemSrc) []

                        back =
                            String.split "\n" body
                    in
                    Expect.equal
                        ( "* head\n- alpha\n  #+begin_src sh\n  echo hi\n  echo mid\n"
                            ++ "  echo bye\n  #+end_src\n  tail\n- beta"
                        , ( "block", 2, 7 )
                        , [ ( 1, 9, "composite:list" ), ( 1, 8, "leaf" ), ( 8, 9, "leaf" ) ]
                        )
                        ( body, regionOf 4 back, scan back )
            , test "a table under an item gains ONE aligned row, still one table" <|
                \_ ->
                    let
                        body =
                            Scan.bodyText (typedAt "B1" (Just 1) "  | e | ff |" itemGrid) []
                    in
                    Expect.equal
                        ( "  |   |    |"
                        , "* head\n- alpha\n  | a | bb |\n  | e | ff |\n  | c | d  |\n- beta"
                        , ( "table", 2, 5 )
                        )
                        ( markerOf 2 itemGrid
                        , body
                        , regionOf 3 (String.split "\n" body)
                        )
            , test "and a drawer under one takes the drawer's answer, :LOGBOOK: included" <|
                \_ ->
                    Expect.equal
                        ( ( ( "drawer", 2, 5 ), "  " )
                        , "* head\n- alpha\n  :LOGBOOK:\n  CLOCK: [2026-08-12 Wed 10:00]\n"
                            ++ "  CLOCK: later\n  :END:\n- beta"
                        )
                        ( ( regionOf 3 itemBook, markerOf 3 itemBook )
                        , Scan.bodyText (typedAt "B1" (Just 2) "  CLOCK: later" itemBook) []
                        )

            -- A CLOSER ASKS FOR WHAT COMES AFTER IT, and inside an item that is
            -- the ITEM: the new line is a sibling landing under the block, where
            -- the prose answer would have put it past the whole list.
            , test "a caret on the nested closer lands after the block, inside the item" <|
                \_ ->
                    Expect.equal
                        ( ( ( "item", 1, 7 ), "- " )
                        , Just 6
                        , "* head\n- alpha\n  #+begin_src sh\n  echo hi\n  echo bye\n"
                            ++ "  #+end_src\n- NEW\n  tail\n- beta"
                        )
                        ( ( regionOf 5 itemSrc, markerOf 5 itemSrc )
                        , Scan.joinLine (model itemSrc) "B1" (Just 4)
                        , Scan.bodyText (typedAt "B1" (Just 4) "- NEW" itemSrc) []
                        )

            -- WHAT NO NESTED REGION CLAIMS IS THE ITEM'S, which is also what
            -- says the walk came back out of the block it went into.
            , test "and a line under the block is the item's own again" <|
                \_ ->
                    Expect.equal ( ( "item", 1, 7 ), "- " )
                        ( regionOf 6 itemSrc, markerOf 6 itemSrc )

            -- THE SCANNER NEEDS NO MATCHING RECURSION, and this is why: it
            -- finds no stop inside an item but a deeper LIST OPENER, so the
            -- block is part of the item's own lines and the draft hangs off the
            -- ITEM.  Stops there would move the grain instead.
            , test "the drawn row hangs off the ITEM, the one stop there is" <|
                \_ ->
                    Expect.equal (Just ( 4, Just "B1" )) (drawnAt "B1" (Just 2) itemSrc)
            ]

        -- ONE WALK, TWO CONSUMERS.  The structure scanner used to hunt an item's
        -- raw lines for openers knowing nothing of `blockName' or `drawerName',
        -- so a bullet inside a nested block was minted as a STOP: the pane drew
        -- it, `f' descended onto it, and `d' then `D' took the block's own
        -- closer out with it.  Three rounds of green suites missed it because no
        -- fixture put a bullet inside a nested region — so these do, each asked
        -- BOTH ways: the stops it yields, and the body every take leaves.
        , describe "the walk says what a nested stop may be"
            [ -- THE SAME THREE STOPS WHATEVER RIDES INSIDE THE ITEM: a block, a
              -- drawer, and a verbatim block holding pipe rows are each one of
              -- the item's own lines.
              test "a bullet, a drawer or a pipe row inside one mints no stop" <|
                \_ ->
                    Expect.equal
                        (List.repeat 3
                            [ ( 1, 6, "composite:list" ), ( 1, 5, "leaf" ), ( 5, 6, "leaf" ) ]
                        )
                        [ scan itemSrcBullet, scan itemBookItem, scan itemSrcPipes ]
            , test "so the item is ONE stop, and taking it takes the whole block" <|
                \_ ->
                    Expect.equal
                        [ "* head"
                        , "* head\n- b"
                        , "* head\n- a\n  #+begin_src sh\n  - not an item\n  #+end_src"
                        ]
                        (takes itemSrcBullet)
            , test "and a line written on that bullet is SOURCE, the block still closing" <|
                \_ ->
                    Expect.equal
                        ("* head\n- a\n  #+begin_src sh\n  - not an item\n  - nor this\n"
                            ++ "  #+end_src\n- b"
                        )
                        (Scan.bodyText (typedAt "B1" (Just 2) "  - nor this" itemSrcBullet) [])
            , test "and taking its item keeps :LOGBOOK: and :END: together" <|
                \_ ->
                    Expect.equal
                        [ "* head"
                        , "* head\n- b"
                        , "* head\n- a\n  :LOGBOOK:\n  - an item\n  :END:"
                        ]
                        (takes itemBookItem)
            , test "and a pipe row in a TOP-LEVEL verbatim block is SOURCE too" <|
                \_ ->
                    Expect.equal [ ( 1, 4, "composite:src" ), ( 2, 3, "leaf" ) ] (scan srcPipes)
            , test "a block inside a nested drawer is the DRAWER's, opener to closer" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 8, "composite:list" ), ( 1, 7, "leaf" ), ( 7, 8, "leaf" ) ]
                        (scan itemDrawerBlock)

            -- THE TOP LEVEL IS ITS OWN PATH and never reached `pushItem', so it
            -- was right already: a block is taken whole there, and a bullet
            -- inside one is a line of the block's own run.
            , test "a bullet inside a TOP-LEVEL block is a run of the block" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 5, "composite:src" ), ( 2, 4, "leaf" ), ( 5, 6, "element" ) ]
                        (scan topBlockBullet)
            , test "and every take over it leaves the block closed" <|
                \_ ->
                    Expect.equal
                        [ "* head\nafter"
                        , "* head\n#+begin_src sh\n#+end_src\nafter"
                        , "* head\n#+begin_src sh\n- not an item\necho\n#+end_src"
                        ]
                        (takes topBlockBullet)

            -- THE WALK'S OWN HALF: the same lines, asked what region holds them.
            -- The scanner reads exactly this, so a stop that disagreed with it
            -- is the bug that was.  Two of them go DEEPER than the region the
            -- line rides in, which is org's greater/lesser split: a drawer
            -- contains elements, so a bullet in one is an item and the block in
            -- one is a block, where `src' suspends the grammar and holds lines.
            , test "and the WALK hands each of those lines to the region holding it" <|
                \_ ->
                    Expect.equal
                        [ ( ( "block", 2, 5 ), "  " )
                        , ( ( "item", 3, 4 ), "  - " )
                        , ( ( "block", 2, 5 ), "  " )
                        , ( ( "block", 3, 6 ), "  " )
                        , ( ( "block", 1, 5 ), "" )
                        , ( ( "block", 1, 4 ), "" )
                        ]
                        [ ( regionOf 3 itemSrcBullet, markerOf 3 itemSrcBullet )
                        , ( regionOf 3 itemBookItem, markerOf 3 itemBookItem )
                        , ( regionOf 3 itemSrcPipes, markerOf 3 itemSrcPipes )
                        , ( regionOf 4 itemDrawerBlock, markerOf 4 itemDrawerBlock )
                        , ( regionOf 2 topBlockBullet, markerOf 2 topBlockBullet )
                        , ( regionOf 2 srcPipes, markerOf 2 srcPipes )
                        ]

            -- ORG'S VERDICT over every stop and every caret of every fixture at
            -- once, and the COUNTS beside it so a sweep that swept nothing
            -- cannot pass.  TWO INDEPENDENT LINE READINGS: an opener without its
            -- closer, and a table that came back as two.
            , test "no take and no write over any of them breaks org's grammar" <|
                \_ ->
                    Expect.equal
                        { unpaired = [], split = [], bodies = [ 27, 25, 68, 231 ] }
                        { unpaired =
                            List.concatMap (unpaired << takes) takeable
                                ++ List.concatMap (unpaired << everyCaret) sweep
                        , split = List.concatMap splits sweep
                        , bodies =
                            [ List.length sweep
                            , List.length takeable
                            , List.sum (List.map (List.length << takes) takeable)
                            , List.sum (List.map (List.length << everyCaret) sweep)
                            ]
                        }

            -- THE ONE ASYMMETRY, AND IT IS OPEN.  A DRAWER IS NO STOP: the
            -- scanner reads `kindAt' like the walk and its `Drawer' arm sends
            -- the line to prose, so a drawer's opener and closer are ordinary
            -- paragraph lines up here and a block inside one splits them into
            -- three stops.  Taking the first leaves `:END:' standing alone.
            -- Closing it means that arm becoming a stop, which reshapes the
            -- pane over every drawer in the corpus — a bigger decision than
            -- this bug.
            , test "a block inside a TOP-LEVEL drawer splits it into three stops" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 2, "element" )
                        , ( 2, 5, "composite:src" )
                        , ( 3, 4, "leaf" )
                        , ( 5, 6, "element" )
                        ]
                        (scan drawerBlock)
            , test "so two of its takes leave a half-drawer, as they always did" <|
                \_ ->
                    Expect.equal
                        [ "* head\n#+begin_src sh\necho\n#+end_src\n:END:"
                        , "* head\n:LOGBOOK:\n:END:"
                        , "* head\n:LOGBOOK:\n#+begin_src sh\n#+end_src\n:END:"
                        , "* head\n:LOGBOOK:\n#+begin_src sh\necho\n#+end_src"
                        ]
                        (takes drawerBlock)
            , test "where the WALK re-enters it and finds the block inside" <|
                \_ ->
                    -- THE ASYMMETRY IS THE STOPS', never the walk's: the drawer
                    -- takes the opener, the closer and the `#+end_src' asking
                    -- for what follows it, and the block takes its own lines.
                    Expect.equal
                        [ "1 drawer 1-6 «»"
                        , "2 block 2-5 «»"
                        , "3 block 2-5 «»"
                        , "4 drawer 1-6 «»"
                        , "5 drawer 1-6 «»"
                        ]
                        (carets drawerBlock)

            -- A REGION TILES THE RUN IT SITS IN, so org's one-blank-line rule
            -- leaves no line inside a list unowned and a caret on that blank
            -- continues the NESTED item under it.  A STOP cut from one gives
            -- the blank back, or the item would draw a line it does not own.
            , test "a blank org keeps inside a nested run is the item's, not a stop's" <|
                \_ ->
                    Expect.equal
                        ( [ ( 1, 6, "composite:list" )
                          , ( 1, 5, "leaf" )
                          , ( 2, 3, "leaf" )
                          , ( 4, 5, "leaf" )
                          , ( 5, 6, "leaf" )
                          ]
                        , "  - x"
                        , ( ( "item", 2, 4 ), "  - " )
                        )
                        ( scan gapRun, textOf "B2" gapRun, ( regionOf 3 gapRun, markerOf 3 gapRun ) )

            -- AND THE PANE'S OWN SHAPE, pinned over a body wearing all five
            -- kinds: a future walk cannot quietly re-cut what a reader steps
            -- through.  The drawer is the paragraph it sits in, which is the
            -- rule that says a region need not be a stop.
            , test "the top-level stop set of a five-kind body is what it was" <|
                \_ ->
                    Expect.equal
                        [ ( 1, 2, "element" )
                        , ( 2, 3, "composite:list" )
                        , ( 2, 3, "leaf" )
                        , ( 3, 4, "composite:table" )
                        , ( 3, 4, "leaf" )
                        , ( 4, 7, "composite:src" )
                        , ( 5, 6, "leaf" )
                        , ( 7, 10, "element" )
                        ]
                        (scan kinds)
            ]

        -- ORG'S OWN GREATER/LESSER SPLIT (`org-element-greater-elements'),
        -- which is the whole rule and closes both holes the item-only walk
        -- left.  A GREATER element CONTAINS elements, so the walk re-enters it:
        -- an ITEM, a DRAWER, and every block org parses the contents of.  A
        -- LESSER one holds none and is OPAQUE: the five VERBATIM blocks that
        -- list leaves out.  A TABLE is greater in org and a leaf here, its only
        -- child being a row, which is what the Table marker spells anyway.
        -- Every container is paired here with every kind it can hold, at TOP
        -- LEVEL and inside an ITEM, and each is asked both ways — the stops it
        -- yields and what every caret writes.
        , describe "org's greater/lesser split — what the walk re-enters"
            [ -- THE CORPUS'S OWN BUG, off
              -- `views/.org-glance/data/gy/m-25044-.../data.org'.  `#+begin_pin'
              -- is a SPECIAL block, so its contents are elements: the list is a
              -- list and the table is a TABLE.  Answered with the block's own
              -- empty line, a caret anywhere in those rows was org's
              -- `[1 table, 21 rows] -> [2 tables, 21 rows]'.
              test "a special block holds a list and a table, each answering for itself" <|
                \_ ->
                    Expect.equal
                        [ "1 block 1-10 «»"
                        , "2 item 2-4 «- »"
                        , "3 item 3-4 «  - »"
                        , "4 block 1-10 «»"
                        , "5 table 5-9 «|                   |                    |»"
                        , "6 table 5-9 «|                   |                    |»"
                        , "7 table 5-9 «|                   |                    |»"
                        , "8 table 5-9 «|                   |                    |»"
                        , "9 block 1-10 «»"
                        ]
                        (carets pinned)
            , test "and a caret in that table writes a row, the table staying one table" <|
                \_ ->
                    let
                        body =
                            Scan.bodyText
                                (typedAt "B0"
                                    (Just 6)
                                    "| Day 23  Tuesday   | Handstand          |"
                                    pinned
                                )
                                []
                    in
                    Expect.equal
                        ( 1
                        , "* DONE [#A] Cali Exercise 1D, Week 4, Day 22 :gym:ARCHIVE:\n"
                            ++ "#+begin_pin\n- Schedule:\n  - [2022-01-05 Wed 19:00 ++1w]\n\n"
                            ++ "| Phase 1 - Week 4  |                    |\n"
                            ++ "|-------------------+--------------------|\n"
                            ++ "| Day               | Exercise           |\n"
                            ++ "| Day 23  Tuesday   | Handstand          |\n"
                            ++ "| Day 22  Monday    | Pike Pushup        |\n"
                            ++ "#+end_pin"
                        )
                        ( tableRuns body, body )

            -- A QUOTE BLOCK IS ORG'S OWN GREATER BLOCK, and it is paired here
            -- with all four kinds.  What each one holds is the kind, never the
            -- block's own empty line.
            , test "a quote block holds a table, a list, a drawer and a block" <|
                \_ ->
                    Expect.equal
                        [ [ "1 block 1-5 «»"
                          , "2 table 2-4 «|   |    |»"
                          , "3 table 2-4 «|   |    |»"
                          , "4 block 1-5 «»"
                          ]
                        , [ "1 block 1-5 «»"
                          , "2 item 2-3 «- »"
                          , "3 item 3-4 «- »"
                          , "4 block 1-5 «»"
                          ]
                        , [ "1 block 1-6 «»"
                          , "2 drawer 2-5 «»"
                          , "3 drawer 2-5 «»"
                          , "4 block 1-6 «»"
                          , "5 block 1-6 «»"
                          ]
                        , [ "1 block 1-6 «»"
                          , "2 block 2-5 «»"
                          , "3 block 2-5 «»"
                          , "4 block 1-6 «»"
                          , "5 block 1-6 «»"
                          ]
                        ]
                        (List.map carets [ quoteGrid, quoteList, quoteBook, quoteSrc ])
            , test "and the stops it yields are the block's own runs, whatever it holds" <|
                \_ ->
                    Expect.equal
                        [ [ ( 1, 5, "composite:quote" ), ( 2, 4, "leaf" ) ]
                        , [ ( 1, 5, "composite:quote" ), ( 2, 4, "leaf" ) ]
                        , [ ( 1, 6, "composite:quote" ), ( 2, 5, "leaf" ) ]
                        , [ ( 1, 6, "composite:quote" ), ( 2, 5, "leaf" ) ]
                        ]
                        (List.map scan [ quoteGrid, quoteList, quoteBook, quoteSrc ])

            -- A DRAWER IS GREATER TOO, which is what makes a `:LOGBOOK:'s own
            -- state lines a list.
            , test "a drawer holds a table, and a drawer holds a list" <|
                \_ ->
                    Expect.equal
                        [ [ "1 drawer 1-5 «»"
                          , "2 table 2-4 «|   |    |»"
                          , "3 table 2-4 «|   |    |»"
                          , "4 drawer 1-5 «»"
                          ]
                        , [ "1 drawer 1-5 «»"
                          , "2 item 2-3 «- »"
                          , "3 item 3-4 «- »"
                          , "4 drawer 1-5 «»"
                          ]
                        ]
                        (List.map carets [ bookGrid, bookList ])

            -- AND THE FIVE ORG NAMES HOLD NOTHING.  One name dropped from that
            -- list turns its fixture's bullet into an item and its pipe row into
            -- a table, which is a bullet spliced into source.
            , test "the five VERBATIM blocks hold no element at all" <|
                \_ ->
                    Expect.equal
                        [ ( "comment", List.repeat 4 "block 1-5 «»" )
                        , ( "example", List.repeat 4 "block 1-5 «»" )
                        , ( "export", List.repeat 4 "block 1-5 «»" )
                        , ( "src", List.repeat 4 "block 1-5 «»" )
                        , ( "verse", List.repeat 4 "block 1-5 «»" )
                        ]
                        (List.map
                            (\( name, body ) ->
                                ( name, List.map (String.dropLeft 2) (carets body) )
                            )
                            verbatims
                        )

            -- THE MEMBERSHIP ITSELF, both polarities in one expectation, over
            -- one body whose only difference is the block's NAME.  The list is
            -- the five `org-element-greater-elements' leaves out; org's LIST
            -- rule, `org-list-forbidden-blocks', names four and spares
            -- `comment', and reading THAT one here left `#+begin_comment'
            -- holding an item and a table.
            , test "which names are opaque is the element list, not the list one" <|
                \_ ->
                    Expect.equal
                        ( [ ( "comment", ( "block", 1, 5 ) )
                          , ( "example", ( "block", 1, 5 ) )
                          , ( "export", ( "block", 1, 5 ) )
                          , ( "src", ( "block", 1, 5 ) )
                          , ( "verse", ( "block", 1, 5 ) )
                          ]
                        , [ ( "center", ( "item", 2, 3 ) )
                          , ( "quote", ( "item", 2, 3 ) )
                          , ( "pin", ( "item", 2, 3 ) )
                          ]
                        )
                        ( List.map (\( name, body ) -> ( name, regionOf 2 body )) verbatims
                        , List.map (\( name, body ) -> ( name, regionOf 2 body )) greaters
                        )

            -- THE NESTED TWINS.  An item is greater and so is what rides inside
            -- it, so the walk goes two deep and the innermost answer stands.
            , test "nested: an item's quote block and its drawer still hold their tables" <|
                \_ ->
                    Expect.equal
                        [ [ "1 item 1-6 «- »"
                          , "2 block 2-6 «  »"
                          , "3 table 3-5 «  |   |    |»"
                          , "4 table 3-5 «  |   |    |»"
                          , "5 item 1-6 «- »"
                          , "6 item 6-7 «- »"
                          ]
                        , [ "1 item 1-6 «- »"
                          , "2 drawer 2-6 «  »"
                          , "3 table 3-5 «  |   |    |»"
                          , "4 table 3-5 «  |   |    |»"
                          , "5 item 1-6 «- »"
                          , "6 item 6-7 «- »"
                          ]
                        ]
                        (List.map carets [ itemQuoteGrid, itemBookGrid ])
            , test "and each is ONE item, the nested region minting no stop" <|
                \_ ->
                    Expect.equal
                        (List.repeat 2
                            [ ( 1, 7, "composite:list" ), ( 1, 6, "leaf" ), ( 6, 7, "leaf" ) ]
                        )
                        (List.map scan [ itemQuoteGrid, itemBookGrid ])

            -- FINDING 2, AND IT IS ORG'S `org-list-struct': a block or a drawer
            -- is one syntactic unit, so the item run steps over it whole rather
            -- than hunting bullets through it.  Cut at the `- b' inside, the
            -- item ended mid-block and taking it carried the opener off without
            -- its closer.
            , test "a block straddling an item boundary is ONE item, not two" <|
                \_ ->
                    Expect.equal
                        ( [ ( 1, 7, "composite:list" ), ( 1, 6, "leaf" ), ( 6, 7, "leaf" ) ]
                        , [ "* head"
                          , "* head\n- c"
                          , "* head\n- a\n  #+begin_src sh\n  echo\n- b\n  #+end_src"
                          ]
                        , [ "1 item 1-6 «- »"
                          , "2 block 2-6 «  »"
                          , "3 block 2-6 «  »"
                          , "4 block 2-6 «  »"
                          , "5 item 1-6 «- »"
                          , "6 item 6-7 «- »"
                          ]
                        )
                        ( scan straddleSrc, takes straddleSrc, carets straddleSrc )
            , test "and the drawer twin is one item too, its own contents elements" <|
                \_ ->
                    -- THE TWO HALVES OF THE RULE IN ONE FIXTURE: the run steps
                    -- over the drawer whole, and the walk then re-enters it, so
                    -- the `- b' the boundary must not be cut at is an ITEM.
                    Expect.equal
                        ( [ ( 1, 7, "composite:list" ), ( 1, 6, "leaf" ), ( 6, 7, "leaf" ) ]
                        , [ "* head"
                          , "* head\n- c"
                          , "* head\n- a\n  :LOGBOOK:\n  clocked\n- b\n  :END:"
                          ]
                        , [ "1 item 1-6 «- »"
                          , "2 drawer 2-6 «  »"
                          , "3 drawer 2-6 «  »"
                          , "4 item 4-5 «- »"
                          , "5 item 1-6 «- »"
                          , "6 item 6-7 «- »"
                          ]
                        )
                        ( scan straddleBook, takes straddleBook, carets straddleBook )

            -- A LINE NO REGION CLAIMS IS PROSE OF ITS OWN, which is `regionAt's
            -- own last arm.  TWO kinds of line reach it: a BLANK between two
            -- regions, and the opener or closer of a greater region the walk has
            -- just re-entered — neither being an interior line, and the second
            -- being how a re-entered region hands itself back.
            , test "a line no region claims is prose of its own, one line wide" <|
                \_ ->
                    Expect.equal
                        [ "1 plain 1-2 «»", "2 plain 2-3 «»", "3 plain 3-4 «»" ]
                        (carets [ "* head", "one", "", "two" ])
            ]
        , describe "the landing — a line, since no id names the new row yet"
            [ test "under a paragraph the text lands a blank on" <|
                \_ ->
                    Expect.equal (Just 3)
                        (Scan.joinLine (model [ "* head", "alpha", "", "beta" ]) "B0" Nothing)
            -- AN ITEM OWES NO BLANK, so its landing is the line under the stop
            -- exactly, where a paragraph's is one past the blank written above.
            , test "an item lands on the line under the stop" <|
                \_ ->
                    Expect.equal (Just 2)
                        (Scan.joinLine (model [ "* head", "- alpha", "- beta" ]) "B1" Nothing)
            , test "and under the headline it leads the body with no blank owed" <|
                \_ ->
                    Expect.equal (Just 1)
                        (Scan.joinLine (model [ "* head", "alpha", "", "beta" ]) "H" Nothing)
            , test "a child owes none" <|
                \_ ->
                    Expect.equal Nothing (Scan.joinLine withKid "C0" Nothing)
            , test "and the row taking that line is the one point lands on" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha", "", "mid", "", "beta" ]
                    in
                    Expect.equal 2 (Scan.placeOfLine { rows = m.rows, at = 0 } 3)
            ]
        ]
