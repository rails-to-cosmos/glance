module ScanTest exposing (suite)

{-| THE SCANNER, ASKED DIRECTLY: the rules the pane rests on that cost a booted
page to reach through the Haskell suite. A body opens with a headline line.
-}

import Array
import Expect
import Body exposing (Kind(..), Row, blank)
import Scan exposing (Grain(..), RegionKind(..))
import Test exposing (Test, describe, test)


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


model : List String -> { rows : List Row, lines : List String }
model lines =
    { rows = Body.rowsFrom lines (List.length lines) [] []
    , lines = lines
    }


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


{-| WHAT EVERY `d`/`D` LEAVES, in row order — ALL of them rather than the one
that was reported.
-}
takes : List String -> List String
takes lines =
    let
        m =
            model lines
    in
    List.map (\r -> Body.bodyText m [ r.id ])
        (List.filter (\r -> r.kind == Para) m.rows)


{-| THE BODIES THAT DO NOT PAIR UP, which a stop set alone cannot say.
-}
unpaired : List String -> List String
unpaired bodies =
    List.filter (not << pairsUp) bodies


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
every `#+begin_` has a `#+end_` and every drawer opener an `:END:`.
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


{-| A drawer's opener, org's charset and all. Spelled again here so the oracle
is the suite's own reading rather than the code it is judging.
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


{-| HOW MANY TABLES A BODY SPELLS, counted as RUNS of pipe rows. A second
independent reading beside `pairsUp`.
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


regionOf : Int -> List String -> ( String, Int, Int )
regionOf line lines =
    let
        reg =
            Scan.regionAt (Array.fromList lines) 1 (List.length lines) line
    in
    ( regionWord reg.kind, reg.from, reg.to )


markerOf : Int -> List String -> String
markerOf line lines =
    Body.markerFor (Array.fromList lines) (Scan.regionAt (Array.fromList lines) 1 (List.length lines) line)


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


{-| EVERY CARET IN A BODY: its region, that region's extent, and its marker.
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
                ++ Body.markerFor lines reg
                ++ "»"
        )
        (List.range 1 (List.length all - 1))


{-| EVERY CARET A READER CAN ACTUALLY PUT DOWN, each writing the box SEEDED
with its region's own marker.
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
                (\o -> Body.bodyText (insertedAt r.id (Just o) "NEW" lines) [])
                (List.range 0 (r.to - r.from - 1))
        )
        (List.filter (\r -> r.kind == Para) m.rows)


splits : List String -> List String
splits lines =
    List.filter (\b -> tableRuns b > tableRuns (String.join "\n" lines)) (everyCaret lines)


leadAt : String -> Maybe Int -> List String -> String
leadAt id caret lines =
    List.foldr
        (\r acc ->
            if r.id == Body.draftId then
                r.text

            else
                acc
        )
        ""
        (Maybe.withDefault [] (Body.drafted (model lines) id caret))


inserted : String -> String -> List String -> { rows : List Row, lines : List String }
inserted id written lines =
    insertedAt id Nothing written lines


{-| The box opened over the stop's line CARET, where `S-RET' opens one.
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
                (Body.insertion m id caret (leadAt id caret lines ++ written))
    }


{-| THE BOX HOLDS THE MARKER AND THE READER EDITS IT: one WHOLE line goes back.
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
    { m | rows = Maybe.withDefault m.rows (Body.insertion m id caret written) }


drawnAt : String -> Maybe Int -> List String -> Maybe ( Int, Maybe String )
drawnAt id caret lines =
    Body.drafted (model lines) id caret
        |> Maybe.andThen (List.filter (\r -> r.id == Body.draftId) >> List.head)
        |> Maybe.map (\r -> ( r.from, r.owner ))


wroteAt : String -> Maybe Int -> String -> List String -> Maybe Int
wroteAt id caret written lines =
    Body.bodyText (insertedAt id caret written lines) []
        |> String.split "\n"
        |> List.indexedMap Tuple.pair
        |> List.filter (\( _, line ) -> String.contains written line)
        |> List.head
        |> Maybe.map Tuple.first


{-| A BODY WITH A CHILD IN IT, whose bytes no gesture here may reach.
-}
withKid : { rows : List Row, lines : List String }
withKid =
    { rows = Body.rowsFrom [ "* head", "mine", "** kid" ] 2 [] [ ( 0, 2, [] ) ]
    , lines = [ "* head", "mine", "** kid" ]
    }


{-| A REAL DOCUMENT off the corpus, blank last line and all; three levels deep.
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


spliced : Int -> List String -> String
spliced i lines =
    String.join "\n" (List.take i chores ++ lines ++ List.drop i chores)


{-| THE CASE AS IT WAS REPORTED: a nested run inside ONE stop.
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


grid : List String
grid =
    [ "* head", "| alpha | beta  |", "|-------+-------|", "| one   | two   |" ]


{-| A TABLE NOBODY ALIGNED, which is what says WHICH width: org pads to the
WIDER.
-}
ragged : List String
ragged =
    [ "* head", "| a | bb |", "| ccc | d |" ]


{-| THE SPEC'S OWN MOTIVATING EXAMPLE: a `#+begin_src` inside a list item.
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


itemGrid : List String
itemGrid =
    [ "* head", "- alpha", "  | a | bb |", "  | c | d  |", "- beta" ]


itemBook : List String
itemBook =
    [ "* head"
    , "- alpha"
    , "  :LOGBOOK:"
    , "  CLOCK: [2026-08-12 Wed 10:00]"
    , "  :END:"
    , "- beta"
    ]


{-| THE REPORTED BUG'S OWN BODY: a bullet inside a `#+begin_src` inside an item.
-}
itemSrcBullet : List String
itemSrcBullet =
    [ "* head", "- a", "  #+begin_src sh", "  - not an item", "  #+end_src", "- b" ]


{-| THE DRAWER TWIN, AND IT IS NOT THE SAME. A drawer is GREATER, so its
contents ARE elements and this bullet IS an item; only the STOPS match.
-}
itemBookItem : List String
itemBookItem =
    [ "* head", "- a", "  :LOGBOOK:", "  - an item", "  :END:", "- b" ]


{-| A PIPE ROW INSIDE A VERBATIM BLOCK IS SOURCE: `src` suspends org's table
grammar. `quoteGrid` is the same pairing where a pipe row CAN be a table.
-}
itemSrcPipes : List String
itemSrcPipes =
    [ "* head", "- a", "  #+begin_src org", "  | x | y |", "  #+end_src", "- b" ]


srcPipes : List String
srcPipes =
    [ "* head", "#+begin_src org", "| a | b |", "#+end_src" ]


{-| A BLOCK INSIDE A DRAWER, which the walk re-enters: the lines are the BLOCK's.
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



-- ORG'S GREATER/LESSER SPLIT: every container paired with every kind it holds,
-- at TOP LEVEL and inside an ITEM, each asked for its stops and for its writes.


{-| THE CORPUS'S OWN SHAPE, off `views/.org-glance/data/gy/m-25044-…/data.org`:
a `#+begin_pin` SPECIAL block holding a list and a table.
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


{-| A GREATER BLOCK holding each kind; an unnamed block is SPECIAL and greater.
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


bookGrid : List String
bookGrid =
    [ "* head", ":LOGBOOK:", "| a | bb |", "| c | d  |", ":END:" ]


bookList : List String
bookList =
    [ "* head", ":RESULTS:", "- a", "- b", ":END:" ]


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
out. `comment` is the one org's `org-list-forbidden-blocks` spares.
-}
verbatims : List ( String, List String )
verbatims =
    List.map (\name -> ( name, blockAround name ))
        [ "comment", "example", "export", "src", "verse" ]


{-| THE OTHER POLARITY over the same body, where the bullet IS an item.
-}
greaters : List ( String, List String )
greaters =
    List.map (\name -> ( name, blockAround name )) [ "center", "quote", "pin" ]


blockAround : String -> List String
blockAround name =
    [ "* head", "#+begin_" ++ name, "- a", "| x | y |", "#+end_" ++ name ]


{-| A BLOCK STRADDLING AN ITEM BOUNDARY: org reads ONE block and one item.
-}
straddleSrc : List String
straddleSrc =
    [ "* head", "- a", "  #+begin_src sh", "  echo", "- b", "  #+end_src", "- c" ]


{-| The drawer twin, which org's own `org-list-struct` steps over the same way.
-}
straddleBook : List String
straddleBook =
    [ "* head", "- a", "  :LOGBOOK:", "  clocked", "- b", "  :END:", "- c" ]


{-| AND THE TOP LEVEL IS ITS OWN PATH: a block is taken whole there already.
-}
topBlockBullet : List String
topBlockBullet =
    [ "* head", "#+begin_src sh", "- not an item", "echo", "#+end_src", "after" ]


{-| ONE BLANK LINE STAYS IN, org's rule, and here it stays in a NESTED run.
-}
gapRun : List String
gapRun =
    [ "* head", "- a", "  - x", "", "  - y", "- b" ]


{-| A DEEPLY-INDENTED ITEM WITH A WRAPPED CONTINUATION: two column-1 bullets.
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


{-| And the ones whose TAKES it must read: a top-level drawer is out, no stop.
-}
takeable : List (List String)
takeable =
    List.filter (\body -> not (List.member body [ bookGrid, bookList ])) sweep


suite : Test
suite =
    describe "Scan"
        [ describe "closers — a commit closes what the typing opened"
            [ test "an opener with no closer earns one, and a line to type on" <|
                \_ -> Expect.equal [ "", "#+end_src" ] (Scan.closers [ "#+begin_src" ])
            , test "a block with content earns no blank" <|
                \_ ->
                    Expect.equal [ "#+end_src" ]
                        (Scan.closers [ "#+begin_src", "code" ])
            , test "text that closes itself earns none" <|
                \_ ->
                    Expect.equal []
                        (Scan.closers [ "#+begin_src", "code", "#+end_src" ])
            , test "NESTING closes innermost first" <|
                \_ ->
                    Expect.equal [ "", "#+end_src", "#+end_quote" ]
                        (Scan.closers [ "#+begin_quote", "#+begin_src" ])
            , test "an inner block already closed leaves only the outer" <|
                \_ ->
                    Expect.equal [ "#+end_quote" ]
                        (Scan.closers [ "#+begin_quote", "#+begin_src", "#+end_src" ])
            , test "an OUTER block holding an inner one is not empty" <|
                \_ ->
                    -- THE CLAIM IS THE MISSING BLANK, so it is asserted apart
                    -- from the closer list above: spelled as the same equality
                    -- it could never fail on its own.
                    Scan.closers [ "#+begin_quote", "#+begin_src", "#+end_src" ]
                        |> List.head
                        |> Expect.notEqual (Just "")
            , test "VERBATIM suspends the grammar: a quote inside src is text" <|
                \_ ->
                    Expect.equal [ "#+end_src" ]
                        (Scan.closers [ "#+begin_src", "#+begin_quote" ])
            , test "and a drawer inside src is text too" <|
                \_ ->
                    Expect.equal [ "#+end_src" ]
                        (Scan.closers [ "#+begin_src", ":LOGBOOK:" ])
            , test "a NON-verbatim block does not suspend it" <|
                \_ ->
                    Expect.equal [ "", "#+end_quote", "#+end_quote" ]
                        (Scan.closers [ "#+begin_quote", "#+begin_quote" ])
            , test "CASE is the opener's own" <|
                \_ -> Expect.equal [ "", "#+END_SRC" ] (Scan.closers [ "#+BEGIN_SRC" ])
            , test "ARGUMENTS are dropped: a closer names only its block" <|
                \_ -> Expect.equal [ "", "#+end_src" ] (Scan.closers [ "#+begin_src elisp -n" ])
            , test "INDENT is the opener's own" <|
                \_ -> Expect.equal [ "", "    #+end_src" ] (Scan.closers [ "    #+begin_src" ])
            , test "a DRAWER earns its END, at its own indent" <|
                \_ -> Expect.equal [ "", "  :END:" ] (Scan.closers [ "  :LOGBOOK:" ])
            , test "a drawer that closes itself earns none" <|
                \_ -> Expect.equal [] (Scan.closers [ ":LOGBOOK:", ":END:" ])
            , test "`:END:' opens nothing" <|
                \_ -> Expect.equal [] (Scan.closers [ ":END:" ])
            , test "a stray `#+end_' opens nothing and closes nothing" <|
                \_ -> Expect.equal [] (Scan.closers [ "#+end_src" ])
            , test "prose earns nothing" <|
                \_ -> Expect.equal [] (Scan.closers [ "just a line", "- an item" ])
            , test "IDEMPOTENT: completing twice is completing once" <|
                \_ ->
                    let
                        once =
                            [ "#+begin_quote", "#+begin_src" ]
                                ++ Scan.closers [ "#+begin_quote", "#+begin_src" ]
                    in
                    Expect.equal [] (Scan.closers once)
            ]
        , describe "listOpener — the corpus's openers, and the one that is not"
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
                        (Body.bodyText (model [ "* head", "alpha", "", "beta" ]) [])
            , test "one changed paragraph replaces its lines and nothing else" <|
                \_ ->
                    Expect.equal "* head\nrewritten\n\nbeta"
                        (Body.bodyText (edited "B0" "rewritten" [ "* head", "alpha", "", "beta" ]) [])
            , test "a replacement of several lines splices them all in" <|
                \_ ->
                    Expect.equal "* head\none\ntwo\n\nbeta"
                        (Body.bodyText (edited "B0" "one\ntwo" [ "* head", "alpha", "", "beta" ]) [])
            , test "a deletion takes its lines, and the blank under them" <|
                \_ ->
                    Expect.equal "* head\nbeta"
                        (Body.bodyText (model [ "* head", "alpha", "", "beta" ]) [ "B0" ])
            , test "the LAST paragraph leaves the blank above it standing" <|
                \_ ->
                    Expect.equal "* head\nalpha\n"
                        (Body.bodyText (model [ "* head", "alpha", "", "beta" ]) [ "B1" ])
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
                    Expect.equal "* head\nfirst\n\nsecond" (Body.bodyText both [])
            ]
        , describe "ONE GRAIN SPEAKS FOR A RANGE"
            [ test "a moved composite silences the leaves under it" <|
                \_ ->
                    -- The list is B0 and its items B1 and B2 over the same lines.
                    Expect.equal "* head\n- rewritten whole"
                        (Body.bodyText
                            (edited "B0" "- rewritten whole" [ "* head", "- alpha", "- beta" ])
                            []
                        )
            , test "a GOING composite silences them, alone or flagged with one" <|
                \_ ->
                    -- Without the silencing the item splices at a range the deletion took out.
                    Expect.equal [ "* head", "* head" ]
                        [ Body.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0" ]
                        , Body.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0", "B1" ]
                        ]
            , test "an edited item under an edited list is the LIST's text" <|
                \_ ->
                    -- Bottom-up ordering cannot survive a leaf splice CHANGING THE LINE COUNT.
                    let
                        under written =
                            let
                                m =
                                    edited "B0" "- whole" [ "* head", "- alpha", "- beta" ]
                            in
                            Body.bodyText
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
                        (Body.bodyText
                            (edited "B2" "- rewritten" [ "* head", "- alpha", "- beta" ])
                            []
                        )
            ]
        , describe "insertion — where a paragraph joins"
            [ test "under the paragraph at point, one blank between" <|
                \_ ->
                    Expect.equal "* head\nalpha\n\nmid\n\nbeta"
                        (Body.bodyText (inserted "B0" "mid" [ "* head", "alpha", "", "beta" ]) [])

            -- THE BLANK BELOW IS DECIDED: the separator is asked of the line.
            , test "and a blank below where what follows is prose" <|
                \_ ->
                    Expect.equal "* head\n- a\n\nnote\n\nafter"
                        (Body.bodyText (inserted "B0" "note" [ "* head", "- a", "after" ]) [])
            , test "the headline's leads the body" <|
                \_ ->
                    Expect.equal "* head\ntop\n\nalpha\n\nbeta"
                        (Body.bodyText (inserted "H" "top" [ "* head", "alpha", "", "beta" ]) [])
            , test "and a body with no block at all is seeded with one" <|
                \_ ->
                    Expect.equal "* head\nfirst\n"
                        (Body.bodyText (inserted "H" "first" [ "* head", "" ]) [])

            -- AN ITEM JOINS STRICTLY BELOW THE STOP, wearing the stop's own prefix
            -- — org's own `M-RET'.  The lead is never in the argument.
            , test "an item's joins STRICTLY BELOW the stop, the run untouched" <|
                \_ ->
                    Expect.equal
                        [ "* head\n- alpha\n- note\n- beta"
                        , "* head\n- alpha\n- note\n- beta\n- gamma"
                        ]
                        [ Body.bodyText
                            (inserted "B1" "note" [ "* head", "- alpha", "- beta" ])
                            []
                        , Body.bodyText
                            (inserted "B1" "note" [ "* head", "- alpha", "- beta", "- gamma" ])
                            []
                        ]

            -- ONE BLANK STAYS INSIDE THE RUN (org's rule), and a sibling goes above it.
            , test "a blank line inside the run stays under the new item" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- note\n\n- beta"
                        (Body.bodyText (inserted "B1" "note" [ "* head", "- alpha", "", "- beta" ]) [])

            -- THE INDENT IS THE CURSOR'S: the nested run's own bottom.
            , test "a nested item's joins the NESTED run, at the stop's indent" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - deep\n  - note\n- beta"
                        (Body.bodyText
                            (inserted "B2" "note" [ "* head", "- alpha", "  - deep", "- beta" ])
                            []
                        )

            -- AN OUTER ITEM'S BOTTOM IS PAST ITS OWN NESTED RUN.
            , test "an item carrying a nested run keeps it above the new sibling" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - deep\n- note"
                        (Body.bodyText (inserted "B1" "note" [ "* head", "- alpha", "  - deep" ]) [])

            -- THE NUMBER IS THE STOP'S, ONE ON, and the item below keeps the one it
            -- had — org's own `M-RET' answer, what `org-list-repair' is for.
            , test "a numbered item takes the stop's number, one on" <|
                \_ ->
                    Expect.equal "* head\n1. alpha\n2. note\n2. beta"
                        (Body.bodyText (inserted "B1" "note" [ "* head", "1. alpha", "2. beta" ]) [])
            , test "and the punctuation is the stop's own" <|
                \_ ->
                    Expect.equal "* head\n1) alpha\n2) note"
                        (Body.bodyText (inserted "B1" "note" [ "* head", "1) alpha" ]) [])

            -- A CHECKBOX COMES ALONG EMPTY, org's own `org-insert-item'.
            , test "a checkbox item's sibling wears an empty box" <|
                \_ ->
                    Expect.equal "* head\n- [X] alpha\n- [ ] note"
                        (Body.bodyText (inserted "B1" "note" [ "* head", "- [X] alpha" ]) [])

            -- WITH NO CARET the answer is a sibling of the STOP, `+' naming no line.
            , test "a table's line rides the table, which stays whole" <|
                \_ ->
                    Expect.equal
                        [ "* head\n| a |\n| b |\n\nnote\n\nafter"
                        , "* head\n| a |\n| b |\n\nnote"
                        ]
                        [ Body.bodyText
                            (inserted "B2" "note" [ "* head", "| a |", "| b |", "", "after" ])
                            []
                        , Body.bodyText
                            (inserted "B1" "note" [ "* head", "| a |", "| b |" ])
                            []
                        ]
            , test "a block's run rides the block, so no prose lands in source" <|
                \_ ->
                    Expect.equal
                        [ "* head\n#+begin_src\nx\n#+end_src\n\nnote\n\nafter"
                        , "* head\n#+begin_src sh\necho one\n#+end_src\n\nnote"
                        ]
                        [ Body.bodyText
                            (inserted "B1"
                                "note"
                                [ "* head", "#+begin_src", "x", "#+end_src", "", "after" ]
                            )
                            []
                        , Body.bodyText
                            (inserted "B1"
                                "note"
                                [ "* head", "#+begin_src sh", "echo one", "#+end_src" ]
                            )
                            []
                        ]
            , test "a child takes none, its bytes being outside this window" <|
                \_ ->
                    Expect.equal Nothing (Body.insertion withKid "C0" Nothing "note")
            , test "nor an id no row wears" <|
                \_ ->
                    Expect.equal Nothing
                        (Body.insertion (model [ "* head", "alpha" ]) "B9" Nothing "note")
            ]

        -- `+' DRAWS THE ROW BEFORE ANYTHING IS WRITTEN, zero-width and passed over.
        , describe "drafted — a paragraph drawn before it is written"
            [ test "the drawn row writes nothing at all" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha", "", "beta" ]

                        rows =
                            Maybe.withDefault m.rows (Body.drafted m "B0" Nothing)
                    in
                    Expect.equal ( 4, "* head\nalpha\n\nbeta" )
                        ( List.length rows, Body.bodyText { m | rows = rows } [] )
            , test "the drawn item writes nothing at all either" <|
                \_ ->
                    -- Its text IS its `was', which is what makes the lead free.
                    let
                        m =
                            model [ "* head", "- alpha" ]

                        rows =
                            Maybe.withDefault m.rows (Body.drafted m "B1" Nothing)
                    in
                    Expect.equal ( 4, "* head\n- alpha" )
                        ( List.length rows, Body.bodyText { m | rows = rows } [] )
            , test "and it stands directly under the stop, inside the list" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "- alpha", "- beta", "", "after" ]
                    in
                    Expect.equal (Just [ "H", "B0", "B1", "D", "B2", "B3" ])
                        (Maybe.map (List.map .id) (Body.drafted m "B1" Nothing))
            , test "a second ask draws one paragraph rather than two" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        once =
                            Maybe.withDefault m.rows (Body.drafted m "B0" Nothing)

                        twice =
                            Maybe.withDefault once (Body.drafted { m | rows = once } "B0" Nothing)
                    in
                    Expect.equal 1
                        (List.length (List.filter (\r -> r.id == Body.draftId) twice))
            , test "and undrafted leaves behind what it found" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        rows =
                            Maybe.withDefault m.rows (Body.drafted m "B0" Nothing)
                    in
                    Expect.equal (List.map .id m.rows)
                        (List.map .id (Body.undrafted { m | rows = rows }))
            ]

        -- WHERE THE CURSOR IS OWED: block ids are POSITIONAL, so a LINE names it.
        , describe "a drafted item is a leaf of the run it joins"
            [ test "it owns what its siblings own, so the composite still draws it" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "- alpha", "  - nested", "- beta" ]

                        drawn =
                            Maybe.withDefault [] (Body.drafted m "B2" Nothing)

                        held =
                            List.map (\r -> ( r.id, r.owner ))
                                (List.filter (\r -> r.id == Body.draftId) drawn)
                    in
                    -- A draft owning NOBODY breaks the `Doc.viewKids' walk.
                    Expect.equal [ ( "D", Just "B1" ) ] held
            , test "and a multi-line item rides inside itself" <|
                \_ ->
                    -- A continuation at column 1 closes the run: org reads a paragraph.
                    Expect.equal "* head\n- alpha\n- one\n  two"
                        (Body.bodyText
                            (inserted "B1" "one\ntwo" [ "* head", "- alpha" ])
                            []
                        )
            , test "where a paragraph keeps its blank lines and no indent" <|
                \_ ->
                    Expect.equal "* head\npara\n\none\ntwo"
                        (Body.bodyText
                            (inserted "B0" "one\ntwo" [ "* head", "para" ])
                            []
                        )
            ]
        -- THE CARET PICKS THE LINE THE LEAD IS SPELLED OFF; the shell sends a NUMBER.
        , describe "the caret's line — which line a lead is spelled off"
            [ test "a deeper line yields its own indent and its own bullet" <|
                \_ ->
                    Expect.equal "  + "
                        (leadAt "B1" (Just 1) [ "* head", "- alpha", "  + beta", "- gamma" ])
            -- AN ABSENT INDEX IS `+' WITH NO BOX OPEN: the stop's first line leads.
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

            -- THE INDEX CHANGES WHICH LINE IS CONSULTED, NEVER WHETHER A LEAD EXISTS.
            , test "a continuation line spells no item, so the stop's first does" <|
                \_ ->
                    Expect.equal ( "- ", "* head\n- alpha\n  more of alpha\n- note\n- beta" )
                        ( leadAt "B1" (Just 1) [ "* head", "- alpha", "  more of alpha", "- beta" ]
                        , Body.bodyText
                            (insertedAt "B1"
                                (Just 1)
                                "note"
                                [ "* head", "- alpha", "  more of alpha", "- beta" ]
                            )
                            []
                        )

            -- A PARAGRAPH'S LINES ARE ONE VALUE: its region answers alike anywhere.
            , test "a paragraph takes none, at any index" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "one", "two" ]
                    in
                    Expect.equal (Body.drafted m "B0" (Just 0)) (Body.drafted m "B0" (Just 2))

            -- THE WRITE MEASURES THE SAME LEAD AS THE DRAW.
            , test "and the write rides continuations under the bullet drawn" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n  - beta\n  - one\n    two\n- gamma"
                        (Body.bodyText
                            (insertedAt "B1"
                                (Just 1)
                                "one\ntwo"
                                [ "* head", "- alpha", "  - beta", "- gamma" ]
                            )
                            []
                        )
            ]

        -- THE CARET'S LINE ANCHORS THE INSERT, over a run nested inside ONE stop.
        , describe "the caret's line — where the sibling lands"
            [ test "a caret mid-run splits it, and what was below stays below" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n    - [ ] Ошейник\n"
                            ++ "  - [X] Записать к грумеру\n  - [ ] Постричь когти"
                        )
                        (Body.bodyText (insertedAt "B1" (Just 6) "Ошейник" pets) [])

            -- THE TWO RULES CAN ONLY BE TOLD APART IN THE MIDDLE of the run.
            , test "at the run's LAST line the split is the bottom" <|
                \_ ->
                    Expect.equal ( Just 10, Just 10 )
                        ( Body.joinLine (model pets) "B1" (Just 8)
                        , Body.joinLine (model pets) "B1" Nothing
                        )
            , test "and the item it writes there wears that line's own indent" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n  - [X] Записать к грумеру\n"
                            ++ "  - [ ] Постричь когти\n  - [ ] Ошейник"
                        )
                        (Body.bodyText (insertedAt "B1" (Just 8) "Ошейник" pets) [])

            -- LINE 0 IS A LINE A READER STOOD ON, so absence is spelled otherwise.
            , test "a caret on line 0 lands under line 0, not past the structure" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n- [ ] Ошейник\n  - [X] Сухой корм\n"
                            ++ "  - [X] Симпарика\n  - [X] Мягкий корм\n  - [X] Для зубов\n"
                            ++ "    - [X] Зубная паста\n    - [X] Зубные щётки Эрику и Юмику\n"
                            ++ "  - [X] Записать к грумеру\n  - [ ] Постричь когти"
                        )
                        (Body.bodyText (insertedAt "B1" (Just 0) "Ошейник" pets) [])

            -- `+' WITH NO BOX OPEN RIDES PAST THE WHOLE STRUCTURE.
            , test "an absent index still rides past the whole structure" <|
                \_ ->
                    Expect.equal
                        ("* head\n- [ ] Пёсики\n  - [X] Сухой корм\n  - [X] Симпарика\n"
                            ++ "  - [X] Мягкий корм\n  - [X] Для зубов\n    - [X] Зубная паста\n"
                            ++ "    - [X] Зубные щётки Эрику и Юмику\n  - [X] Записать к грумеру\n"
                            ++ "  - [ ] Постричь когти\n- [ ] Ошейник"
                        )
                        (Body.bodyText (inserted "B1" "Ошейник" pets) [])

            -- ONE LINE, TWO READERS: the draw and the splice must agree.
            , test "the drawn draft and the written line are one line" <|
                \_ ->
                    Expect.equal ( Just ( 8, Just "B5" ), Just 8, Just 8 )
                        ( drawnAt "B1" (Just 6) pets
                        , wroteAt "B1" (Just 6) "Ошейник" pets
                        , Body.joinLine (model pets) "B1" (Just 6)
                        )

            -- AND IT STANDS IN THE ROW ORDER ITS BYTES ARE IN.
            , test "and in the row order its bytes are in" <|
                \_ ->
                    Expect.equal
                        (Just [ "H", "B0", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "D", "B8", "B9" ])
                        (Maybe.map (List.map .id) (Body.drafted (model pets) "B1" (Just 6)))
            ]
        -- A LINE INSIDE A LIST BELONGS TO AN ITEM however wide the stop laid over
        -- it, so the caret is what decides.
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
                        -- 16 is the lens's own `ownLines'; the blank last line is outside it.
                        (scanOwn 16 chores)

            -- A SHALLOWER ITEM UNDER A DEEPER ONE ENDS THE DEEPER RUN EXACTLY.
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

            -- The caret's line decides, so the width of the box stops mattering.
            , test "S-RET in a box over the whole list writes the caret line's own item" <|
                \_ ->
                    Expect.equal ( "    - [ ] ", spliced 8 [ "    - [ ] Ошейник" ] )
                        ( leadAt "B0" (Just 6) chores
                        , Body.bodyText (insertedAt "B0" (Just 6) "Ошейник" chores) []
                        )
            , test "and the row is drawn in the run it was written into" <|
                \_ ->
                    Expect.equal (Just ( 8, Just "B5" ))
                        (drawnAt "B0" (Just 6) chores)

            -- THE RULE STAYS THE GRAIN'S WHERE NOTHING NAMED A LINE.
            , test "and `+' with no box open still lands a paragraph past the list" <|
                \_ ->
                    Expect.equal ( "", spliced 16 [ "", "note" ] )
                        ( leadAt "B0" Nothing chores
                        , Body.bodyText (inserted "B0" "note" chores) []
                        )

            -- AND THE REGION IS ASKED ABOUT, never the line alone: a source block can
            -- hold a line that OPENS LIKE AN ITEM, and the block's answer wins.
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
                        , Body.bodyText (insertedAt "B1" (Just 0) "note" src) []
                        )
            -- A CONTINUATION LINE NAMES NO ITEM: the rung HOLDING the line spells it.
            , test "a caret on a continuation takes the rung that holds it" <|
                \_ ->
                    let
                        riding =
                            [ "* head", "- alpha", "  - deep", "    inside deep", "- beta" ]
                    in
                    Expect.equal ( "  - ", "* head\n- alpha\n  - deep\n    inside deep\n  - note\n- beta" )
                        ( leadAt "B0" (Just 2) riding
                        , Body.bodyText (insertedAt "B0" (Just 2) "note" riding) []
                        )
            ]
        -- ONE QUESTION — WHICH REGION HOLDS THE CARET — AND ONE ANSWER PER REGION.
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

            -- `- NEW' spliced between two source lines is a block cut in half.
            , test "a caret inside a #+begin_src adds an EMPTY line inside it" <|
                \_ ->
                    Expect.equal ( "", Just ( 3, Just "B1" ) )
                        ( leadAt "B1" (Just 0) srcBlock
                        , drawnAt "B1" (Just 0) srcBlock
                        )
            , test "and what is written there lands inside, the block still closing" <|
                \_ ->
                    Expect.equal "* head\n#+begin_src sh\necho one\necho mid\necho two\n#+end_src"
                        (Body.bodyText (typedAt "B1" (Just 0) "echo mid" srcBlock) [])

            -- A DRAWER IS NO STOP: the scanner leaves it inside its paragraph.
            , test "a drawer takes the same answer, :LOGBOOK: included" <|
                \_ ->
                    Expect.equal
                        ( ""
                        , "* head\nnotes\n:LOGBOOK:\nCLOCK: [2026-08-12 Wed 10:00]\nCLOCK: later\n:END:"
                        )
                        ( leadAt "B0" (Just 2) logbook
                        , Body.bodyText (typedAt "B0" (Just 2) "CLOCK: later" logbook) []
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

            -- ORG'S OWN ALIGNMENT, measured off the rows the table already spells.
            , test "a table's is an EMPTY ROW at the table's own widths" <|
                \_ ->
                    Expect.equal "|       |       |" (markerOf 1 grid)
            , test "and the RULE ROWS are out of the measurement" <|
                \_ ->
                    -- Measuring the rule row would pad every new row to its dashes.
                    Expect.equal "|   |   |"
                        (markerOf 1 [ "* head", "| a | b |", "|-------+-------|", "| c | d |" ])

            -- A RAGGED TABLE is what says WHICH width: org pads to the WIDER.
            , test "a ragged table pads each column to its WIDEST cell" <|
                \_ ->
                    Expect.equal "|     |    |" (markerOf 1 ragged)
            -- A MARKER IS A LEAD AND POINT GOES AFTER IT — except a table row.
            , test "and point lands where the reader types, one space into a row" <|
                \_ ->
                    Expect.equal [ 2, 6, 0, 2, 4, 1 ]
                        (List.map Body.caretIn
                            [ "- ", "- [ ] ", "", "|   |   |", "  |   |    |", "||" ]
                        )
            , test "the row it writes keeps the table one table" <|
                \_ ->
                    let
                        body =
                            Body.bodyText (typedAt "B0" (Just 2) "| three | four  |" grid) []
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

            -- A CLOSER IS THE REGION'S LAST LINE and a caret on it asks for what comes
            -- AFTER.  A TABLE HAS NO CLOSER, so its last row keeps the new row inside.
            , test "a caret on #+end_X lands AFTER the block" <|
                \_ ->
                    let
                        src =
                            [ "* head", "#+begin_src sh", "echo one", "#+end_src" ]
                    in
                    Expect.equal ( "", "* head\n#+begin_src sh\necho one\n#+end_src\n\nafter" )
                        ( leadAt "B0" (Just 2) src
                        , Body.bodyText (typedAt "B0" (Just 2) "after" src) []
                        )
            , test "and one on :END: lands after the drawer" <|
                \_ ->
                    let
                        book =
                            [ "* head", ":LOGBOOK:", "clocked", ":END:" ]
                    in
                    Expect.equal "* head\n:LOGBOOK:\nclocked\n:END:\n\nafter"
                        (Body.bodyText (typedAt "B0" (Just 2) "after" book) [])
            , test "where a table's last row is a line INSIDE it" <|
                \_ ->
                    -- Same index, two regions, two answers.
                    Expect.equal ( Just 4, Just 5 )
                        ( Body.joinLine (model grid) "B0" (Just 2)
                        , Body.joinLine
                            (model [ "* head", "#+begin_src sh", "echo one", "#+end_src" ])
                            "B0"
                            (Just 2)
                        )

            -- ORG'S OWN `M-RET': the number continues off the CARET'S line.
            , test "a numbered list continues from the caret's own number" <|
                \_ ->
                    let
                        nums =
                            [ "* head", "1. one", "2. two", "3. three" ]
                    in
                    Expect.equal ( "3. ", "* head\n1. one\n2. two\n3. mid\n3. three" )
                        ( leadAt "B0" (Just 1) nums
                        , Body.bodyText (insertedAt "B0" (Just 1) "mid" nums) []
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

            -- AND THE RULE WIDENS NOTHING: text org declines takes the PROSE answer.
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
                    -- Out of the region AND out of the measurement.
                    Expect.equal ( ( "table", 1, 2 ), "|   |    |" )
                        ( regionOf 1 [ "* head", "| a | bb |", "", "| ccccc |" ]
                        , markerOf 1 [ "* head", "| a | bb |", "", "| ccccc |" ]
                        )
            , test "and `:a:b:' is no drawer — org's own charset holds" <|
                \_ ->
                    -- EVERY FIXTURE CLOSES, or the charset would be asserting nothing.
                    Expect.equal
                        [ ( "plain", 1, 4 ), ( "plain", 1, 4 ), ( "drawer", 1, 4 ) ]
                        [ regionOf 1 [ "* head", ":a:b:", "x", ":END:" ]
                        , regionOf 1 [ "* head", ":MY DRAWER:", "x", ":END:" ]
                        , regionOf 1 [ "* head", ":ok-name_1:", "x", ":END:" ]
                        ]

            -- ORG CLOSES A DRAWER ON `:end:' TOO, folding where the opener does not.
            , test "and a lowercase :end: closes one" <|
                \_ ->
                    Expect.equal ( "drawer", 1, 4 )
                        (regionOf 2 [ "* head", ":LOGBOOK:", "clocked", ":end:" ])

            -- A CONTINUATION LINE TAKES THE ITEM HOLDING IT, so `S-RET' on wrapped prose
            -- cuts the paragraph.  Today's answer, pinned so a change names it.
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
                        (Body.bodyText (typedAt "B2" (Just 1) "  - NEW" wrapRun) [])
            ]

        -- AN ITEM'S LINES ARE A BODY OF THEIR OWN, so every kind gets its nested
        -- twin here where the cases before it asked TOP LEVEL alone.
        , describe "a region nested inside an ITEM — the walk re-enters it"
            [ test "a #+begin_src under an item is the BLOCK, not the item" <|
                \_ ->
                    Expect.equal ( ( "block", 2, 6 ), "  " )
                        ( regionOf 3 itemSrc, markerOf 3 itemSrc )
            , test "so the line lands inside it and the block still closes" <|
                \_ ->
                    let
                        body =
                            Body.bodyText (typedAt "B1" (Just 2) "  echo mid" itemSrc) []

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
                            Body.bodyText (typedAt "B1" (Just 1) "  | e | ff |" itemGrid) []
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
                        , Body.bodyText (typedAt "B1" (Just 2) "  CLOCK: later" itemBook) []
                        )

            -- A CLOSER ASKS FOR WHAT COMES AFTER IT — inside an item, the ITEM.
            , test "a caret on the nested closer lands after the block, inside the item" <|
                \_ ->
                    Expect.equal
                        ( ( ( "item", 1, 7 ), "- " )
                        , Just 6
                        , "* head\n- alpha\n  #+begin_src sh\n  echo hi\n  echo bye\n"
                            ++ "  #+end_src\n- NEW\n  tail\n- beta"
                        )
                        ( ( regionOf 5 itemSrc, markerOf 5 itemSrc )
                        , Body.joinLine (model itemSrc) "B1" (Just 4)
                        , Body.bodyText (typedAt "B1" (Just 4) "- NEW" itemSrc) []
                        )

            -- WHAT NO NESTED REGION CLAIMS IS THE ITEM'S.
            , test "and a line under the block is the item's own again" <|
                \_ ->
                    Expect.equal ( ( "item", 1, 7 ), "- " )
                        ( regionOf 6 itemSrc, markerOf 6 itemSrc )

            -- THE SCANNER NEEDS NO MATCHING RECURSION; stops would move the grain.
            , test "the drawn row hangs off the ITEM, the one stop there is" <|
                \_ ->
                    Expect.equal (Just ( 4, Just "B1" )) (drawnAt "B1" (Just 2) itemSrc)
            ]

        -- ONE WALK, TWO CONSUMERS.  These fixtures put a bullet inside a nested
        -- region, each asked BOTH ways: the stops, and the body every take leaves.
        , describe "the walk says what a nested stop may be"
            [ -- THE SAME THREE STOPS WHATEVER RIDES INSIDE THE ITEM: a block, a
              -- drawer or a verbatim block holding pipe rows are the item's own lines.
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
                        (Body.bodyText (typedAt "B1" (Just 2) "  - nor this" itemSrcBullet) [])
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

            -- THE TOP LEVEL IS ITS OWN PATH: a block is taken whole there.
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

            -- THE WALK'S OWN HALF: the same lines, asked what region holds them.  Two
            -- go DEEPER than the region the line rides in, which is greater/lesser.
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

            -- ORG'S VERDICT over every fixture at once, the COUNTS beside it.
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

            -- THE ONE ASYMMETRY, AND IT IS OPEN.  A DRAWER IS NO STOP at the top level:
            -- its opener and its closer are ordinary paragraph lines up here.
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
                    -- THE ASYMMETRY IS THE STOPS', never the walk's.
                    Expect.equal
                        [ "1 drawer 1-6 «»"
                        , "2 block 2-5 «»"
                        , "3 block 2-5 «»"
                        , "4 drawer 1-6 «»"
                        , "5 drawer 1-6 «»"
                        ]
                        (carets drawerBlock)

            -- A REGION TILES THE RUN IT SITS IN; a STOP cut from one gives the blank back.
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

            -- AND THE PANE'S OWN SHAPE, pinned over a body wearing all five kinds.
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

        -- ORG'S OWN GREATER/LESSER SPLIT (`org-element-greater-elements'): a GREATER
        -- element CONTAINS elements and the walk re-enters it, a LESSER one is
        -- OPAQUE.  A TABLE is greater in org and a leaf here, its child being a row.
        , describe "org's greater/lesser split — what the walk re-enters"
            [ -- THE CORPUS'S OWN BUG, off
              -- `views/.org-glance/data/gy/m-25044-.../data.org'.  `#+begin_pin' is
              -- SPECIAL, so its contents are elements: the table is a TABLE.
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
                            Body.bodyText
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

            -- A QUOTE BLOCK IS ORG'S OWN GREATER BLOCK, paired with all four kinds.
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

            -- A DRAWER IS GREATER TOO, which makes a `:LOGBOOK:'s state lines a list.
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

            -- AND THE FIVE ORG NAMES HOLD NOTHING; one dropped is a bullet in source.
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

            -- THE MEMBERSHIP ITSELF, both polarities over one body differing only in the
            -- block's NAME.  `org-list-forbidden-blocks' spares `comment'.
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

            -- THE NESTED TWINS: the walk goes two deep and the innermost answer stands.
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

            -- ORG'S `org-list-struct': a block or a drawer is one syntactic unit.
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
                    -- THE TWO HALVES OF THE RULE IN ONE FIXTURE.
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

            -- A LINE NO REGION CLAIMS IS PROSE OF ITS OWN, `regionAt's last arm: a BLANK
            -- between regions, or a re-entered region's own edge lines.
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
                        (Body.joinLine (model [ "* head", "alpha", "", "beta" ]) "B0" Nothing)
            -- AN ITEM OWES NO BLANK: its landing is the line under the stop.
            , test "an item lands on the line under the stop" <|
                \_ ->
                    Expect.equal (Just 2)
                        (Body.joinLine (model [ "* head", "- alpha", "- beta" ]) "B1" Nothing)
            , test "and under the headline it leads the body with no blank owed" <|
                \_ ->
                    Expect.equal (Just 1)
                        (Body.joinLine (model [ "* head", "alpha", "", "beta" ]) "H" Nothing)
            , test "a child owes none" <|
                \_ ->
                    Expect.equal Nothing (Body.joinLine withKid "C0" Nothing)
            , test "and the row taking that line is the one point lands on" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha", "", "mid", "", "beta" ]
                    in
                    Expect.equal 2 (Body.placeOfLine { rows = m.rows, at = 0 } 3)
            ]
        ]
