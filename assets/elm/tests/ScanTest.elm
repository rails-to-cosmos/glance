module ScanTest exposing (suite)

{-| THE SCANNER, ASKED DIRECTLY. Every case here is a rule the pane's behaviour
rests on that costs a booted page to reach through the Haskell suite — org's
one-blank-line rule, a block closing by NAME, an indented `*` being an item
where a column-1 one is a headline, and the splice's "one grain speaks for a
range".

The body always opens with a headline line, because `blocksIn` starts at index
1: the line the entry wears is the sheet's headline row, never a paragraph.

-}

import Expect
import Scan exposing (Grain(..), Kind(..), Row, blank)
import Test exposing (Test, describe, test)


{-| A body as the sheet holds it: lines, and how many of them are this entry's
own rather than a child's.
-}
scan : List String -> List ( Int, Int, String )
scan lines =
    List.map (\b -> ( b.from, b.to, grainOf b.grain ++ Maybe.withDefault "" (Maybe.map ((++) ":") b.name) ))
        (Scan.blocksIn lines (List.length lines))


scanOwn : Int -> List String -> List ( Int, Int, String )
scanOwn own lines =
    List.map (\b -> ( b.from, b.to, grainOf b.grain ++ Maybe.withDefault "" (Maybe.map ((++) ":") b.name) ))
        (Scan.blocksIn lines own)


{-| The leaf ownership, which is the grain LADDER: `up` is the IMMEDIATE owner.
-}
owners : List String -> List (Maybe Int)
owners lines =
    List.map .up (Scan.blocksIn lines (List.length lines))


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


{-| The same, with a paragraph spelling WRITTEN joined under ROW — what \`+' does
before a flush. The rows come back unchanged where the stop takes none.
-}
inserted : String -> String -> List String -> { rows : List Row, lines : List String }
inserted id written lines =
    let
        m =
            model lines
    in
    { m | rows = Maybe.withDefault m.rows (Scan.insertion m id written) }


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
            , test "and a GOING composite silences them the same way" <|
                \_ ->
                    Expect.equal "* head"
                        (Scan.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0" ])
            , test "a list and an item under it, both flagged, are ONE deletion" <|
                \_ ->
                    -- The rule's only real occasion: without the silencing the
                    -- item would splice at a range the list's own deletion has
                    -- already taken out.
                    Expect.equal "* head"
                        (Scan.bodyText (model [ "* head", "- alpha", "- beta" ]) [ "B0", "B1" ])
            , test "an item whose edit GROWS it cannot shift the list's own range" <|
                \_ ->
                    -- The rule's real occasion.  Bottom-up ordering alone keeps
                    -- most of these right, because the list's range covers the
                    -- item's and lands last; what it cannot survive is a leaf
                    -- splice that CHANGES THE LINE COUNT under it.
                    let
                        m =
                            edited "B0" "- whole" [ "* head", "- alpha", "- beta" ]

                        both =
                            { m
                                | rows =
                                    List.map
                                        (\r ->
                                            if r.id == "B1" then
                                                { r | text = "- item\n- extra" }

                                            else
                                                r
                                        )
                                        m.rows
                            }
                    in
                    Expect.equal "* head\n- whole" (Scan.bodyText both [])
            , test "and an edited item under an edited list is the LIST's text" <|
                \_ ->
                    let
                        m =
                            edited "B0" "- whole" [ "* head", "- alpha", "- beta" ]

                        both =
                            { m
                                | rows =
                                    List.map
                                        (\r ->
                                            if r.id == "B1" then
                                                { r | text = "- item" }

                                            else
                                                r
                                        )
                                        m.rows
                            }
                    in
                    Expect.equal "* head\n- whole" (Scan.bodyText both [])
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

            -- AN ITEM JOINS ITS RUN'S BOTTOM, wearing the stop's own prefix.
            -- The typed text is what the READER typed, so the lead appears in
            -- the expectation and never in the argument.
            , test "an item's joins STRICTLY BELOW the stop" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- note\n- beta"
                        (Scan.bodyText (inserted "B1" "note" [ "* head", "- alpha", "- beta" ]) [])

            -- ORG'S OWN `M-RET': the reader walked to an item and the new one
            -- belongs under THAT one, never at a bottom they would walk back
            -- up from.  Two items below it stay where they are.
            , test "and the run below it is untouched, however long" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- note\n- beta\n- gamma"
                        (Scan.bodyText
                            (inserted "B1" "note" [ "* head", "- alpha", "- beta", "- gamma" ])
                            []
                        )

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

            -- THE OLD LANDING IS ONE `b' AWAY, and these are the bytes it wrote
            -- from the leaf before the grain decided.
            , test "the COMPOSITE still lands a paragraph past the whole list" <|
                \_ ->
                    Expect.equal "* head\n- alpha\n- beta\n\nnote\n\nafter"
                        (Scan.bodyText
                            (inserted "B0" "note" [ "* head", "- alpha", "- beta", "", "after" ])
                            []
                        )

            -- A TABLE LINE AND A BLOCK RUN KEEP THE COMPOSITE'S LANDING: a pipe
            -- row's cells sit BETWEEN pipes and a source line's grammar is X's,
            -- so neither is a PREFIX this page can spell — and grown in place,
            -- org would cut the table or take the prose for source.
            , test "a table's line rides the table, which stays whole" <|
                \_ ->
                    Expect.equal "* head\n| a |\n| b |\n\nnote\n\nafter"
                        (Scan.bodyText
                            (inserted "B2" "note" [ "* head", "| a |", "| b |", "", "after" ])
                            []
                        )
            , test "a block's run rides the block, so no prose lands in source" <|
                \_ ->
                    Expect.equal "* head\n#+begin_src\nx\n#+end_src\n\nnote\n\nafter"
                        (Scan.bodyText
                            (inserted "B1"
                                "note"
                                [ "* head", "#+begin_src", "x", "#+end_src", "", "after" ]
                            )
                            []
                        )
            , test "a child takes none, its bytes being outside this window" <|
                \_ ->
                    Expect.equal Nothing
                        (Scan.insertion
                            { rows = Scan.rowsFrom [ "* head", "mine", "** kid" ] 2 [] [ ( 0, 2, [] ) ]
                            , lines = [ "* head", "mine", "** kid" ]
                            }
                            "C0"
                            "note"
                        )
            , test "nor an id no row wears" <|
                \_ ->
                    Expect.equal Nothing (Scan.insertion (model [ "* head", "alpha" ]) "B9" "note")
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
                            Maybe.withDefault m.rows (Scan.drafted m "B0")
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
                            Maybe.withDefault m.rows (Scan.drafted m "B1")
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
                        (Maybe.map (List.map .id) (Scan.drafted m "B1"))
            , test "a second ask draws one paragraph rather than two" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        once =
                            Maybe.withDefault m.rows (Scan.drafted m "B0")

                        twice =
                            Maybe.withDefault once (Scan.drafted { m | rows = once } "B0")
                    in
                    Expect.equal 1
                        (List.length (List.filter (\r -> r.id == Scan.draftId) twice))
            , test "and undrafted leaves behind what it found" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha" ]

                        rows =
                            Maybe.withDefault m.rows (Scan.drafted m "B0")
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
                            Maybe.withDefault [] (Scan.drafted m "B2")

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
        , describe "the landing — a line, since no id names the new row yet"
            [ test "under a paragraph the text lands a blank on" <|
                \_ ->
                    Expect.equal (Just 3)
                        (Scan.joinLine (model [ "* head", "alpha", "", "beta" ]) "B0")
            -- AN ITEM OWES NO BLANK, so its landing is the line under the stop
            -- exactly, where a paragraph's is one past the blank written above.
            , test "an item lands on the line under the stop" <|
                \_ ->
                    Expect.equal (Just 2)
                        (Scan.joinLine (model [ "* head", "- alpha", "- beta" ]) "B1")
            , test "and under the headline it leads the body with no blank owed" <|
                \_ ->
                    Expect.equal (Just 1)
                        (Scan.joinLine (model [ "* head", "alpha", "", "beta" ]) "H")
            , test "a child owes none" <|
                \_ ->
                    Expect.equal Nothing
                        (Scan.joinLine
                            { rows = Scan.rowsFrom [ "* head", "mine", "** kid" ] 2 [] [ ( 0, 2, [] ) ]
                            , lines = [ "* head", "mine", "** kid" ]
                            }
                            "C0"
                        )
            , test "and the row taking that line is the one point lands on" <|
                \_ ->
                    let
                        m =
                            model [ "* head", "alpha", "", "mid", "", "beta" ]
                    in
                    Expect.equal 2 (Scan.placeOfLine { rows = m.rows, at = 0 } 3)
            ]
        ]
