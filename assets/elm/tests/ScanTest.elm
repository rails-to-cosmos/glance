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
        ]
