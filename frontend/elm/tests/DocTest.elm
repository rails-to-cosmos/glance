module DocTest exposing (suite)

{-| THE EDITOR'S PURE CORE, ASKED DIRECTLY: the derived-box rollup, the org
statistics cookie, and the hide-done scope -- the math behind the last four
`Doc.elm' bug-fixes, which the scanner suite never reaches because it never
imports `Doc'.
-}

import Array
import Body exposing (Row)
import Doc
    exposing
        ( BoxFace(..)
        , Model
        , compactedRun
        , cookieIn
        , cookieKind
        , empty
        , findCookie
        , hiddenDone
        , rollUp
        )
import Expect
import Scan exposing (Grain(..))
import Set
import Test exposing (Test, describe, test)


{-| A DOC MODEL over LINES, seeded the way `fillD' seeds one: the rows scanned
whole and the array the line reader (`lineOf', `openerAt') rests on.
-}
docModel : List String -> Model
docModel lines =
    { empty
        | lines = lines
        , arr = Array.fromList lines
        , rows = Body.rowsFrom lines (List.length lines) [] []
    }


{-| The same model with every list run switched into the hide-done mode.
-}
hidingAllRoots : List String -> Model
hidingAllRoots lines =
    let
        base =
            docModel lines
    in
    { base | hideDone = Set.fromList (rootIds base) }


{-| Every list run's OUTERMOST composite -- the id a hide-done toggle keys on.
-}
rootIds : Model -> List String
rootIds m =
    List.filterMap
        (\r ->
            if r.grain == Composite && r.name == Just "list" then
                Just r.id

            else
                Nothing
        )
        m.rows


rootRow : Model -> Maybe Row
rootRow m =
    List.head (List.filter (\r -> r.grain == Composite && r.name == Just "list") m.rows)


{-| The id of the LEAF item on line LN -- the composite over the same span,
being no leaf, is passed by.
-}
leafIdAt : Model -> Int -> String
leafIdAt m ln =
    m.rows
        |> List.filter (\r -> r.grain == Leaf && r.from == ln)
        |> List.head
        |> Maybe.map .id
        |> Maybe.withDefault "?"


{-| A part-done run: one ticked box, one empty.
-}
mixed : List String
mixed =
    [ "* head", "- [X] a", "- [ ] b" ]


{-| A run whose whole checkbox subtree is done.
-}
allDone : List String
allDone =
    [ "* head", "- [X] a", "- [X] b" ]


suite : Test
suite =
    describe "Doc — the editor's pure core"
        [ describe "rollUp — a parent's face from its children's"
            [ test "every child empty rolls up empty" <|
                \_ -> Expect.equal BoxEmpty (rollUp [ BoxEmpty, BoxEmpty ])
            , test "every child full rolls up full" <|
                \_ -> Expect.equal BoxFull (rollUp [ BoxFull, BoxFull ])
            , test "a disagreement rolls up partial" <|
                \_ -> Expect.equal BoxPart (rollUp [ BoxFull, BoxEmpty ])
            , test "and any child already partial makes the parent partial" <|
                \_ -> Expect.equal BoxPart (rollUp [ BoxFull, BoxPart ])
            , test "the vacuous list rolls up full — `List.all' on empty" <|
                \_ -> Expect.equal BoxFull (rollUp [])
            ]
        , describe "cookieIn / cookieKind — org's statistics cookie"
            [ test "an empty fraction cookie `[/]' is found whole" <|
                \_ -> Expect.equal (Just ( 0, 3, False )) (cookieIn "[/]")
            , test "an empty percent cookie `[%]' reads as percent" <|
                \_ -> Expect.equal (Just ( 0, 3, True )) (cookieIn "[%]")
            , test "a filled fraction `[1/2]' is found, its span exact" <|
                \_ -> Expect.equal (Just ( 0, 5, False )) (cookieIn "[1/2]")
            , test "the checkbox bracket is SKIPPED to the cookie past it" <|
                \_ -> Expect.equal (Just ( 6, 11, False )) (cookieIn "[X] a [1/2]")
            , test "a box with no cookie behind it finds none" <|
                \_ -> Expect.equal Nothing (cookieIn "[X] just a box")
            , test "`foo' is no cookie body" <|
                \_ -> Expect.equal Nothing (cookieKind "foo")
            , test "`50%' is a percent body, `1/2' a fraction" <|
                \_ ->
                    Expect.equal ( Just True, Just False )
                        ( cookieKind "50%", cookieKind "1/2" )
            , test "findCookie's `from' steps past the first cookie to the next" <|
                \_ -> Expect.equal (Just ( 5, 10, False )) (findCookie "[1/2][3/4]" 1)
            ]
        , describe "hiddenDone — the hide-done run's swallowed rows"
            [ test "the mode off hides nothing" <|
                \_ -> Expect.equal Set.empty (hiddenDone (docModel mixed))
            , test "a ticked leaf under an on run is hidden; an empty one is not" <|
                \_ ->
                    let
                        m =
                            hidingAllRoots mixed
                    in
                    Expect.equal ( True, False )
                        ( Set.member (leafIdAt m 1) (hiddenDone m)
                        , Set.member (leafIdAt m 2) (hiddenDone m)
                        )
            , test "a run only partly done keeps its root composite standing" <|
                \_ ->
                    let
                        m =
                            hidingAllRoots mixed
                    in
                    Expect.equal (Just False)
                        (Maybe.map (\root -> Set.member root.id (hiddenDone m)) (rootRow m))
            , test "a WHOLLY done run vanishes container and all — root plus both leaves" <|
                \_ ->
                    Expect.equal 3 (Set.size (hiddenDone (hidingAllRoots allDone)))
            ]
        , describe "compactedRun — a run compacted but not emptied"
            [ test "a part-done on run is compacted (its spine goes dashed)" <|
                \_ ->
                    let
                        m =
                            hidingAllRoots mixed
                    in
                    Expect.equal (Just True)
                        (Maybe.map (compactedRun m (hiddenDone m)) (rootRow m))
            , test "a wholly-done run is gone, not compacted" <|
                \_ ->
                    let
                        m =
                            hidingAllRoots allDone
                    in
                    Expect.equal (Just False)
                        (Maybe.map (compactedRun m (hiddenDone m)) (rootRow m))
            , test "the mode off compacts nothing" <|
                \_ ->
                    let
                        m =
                            docModel mixed
                    in
                    Expect.equal (Just False)
                        (Maybe.map (compactedRun m (hiddenDone m)) (rootRow m))
            ]
        , describe "planEntries — every settable slot draws, unset where the file gave none"
            [ test "no plan and no summon still draws both slots unset" <|
                \_ ->
                    Expect.equal [ ( "SCHEDULED", "" ), ( "DEADLINE", "" ) ]
                        (Body.planEntries [ "SCHEDULED", "DEADLINE" ] [] Nothing)
            , test "a slot the file filled carries its value, in slots order" <|
                \_ ->
                    Expect.equal [ ( "SCHEDULED", "" ), ( "DEADLINE", "<d>" ) ]
                        (Body.planEntries [ "SCHEDULED", "DEADLINE" ] [ ( "DEADLINE", "<d>" ) ] Nothing)
            , test "an entry beyond the slots (CLOSED) is kept after them" <|
                \_ ->
                    Expect.equal [ ( "SCHEDULED", "<s>" ), ( "DEADLINE", "" ), ( "CLOSED", "<c>" ) ]
                        (Body.planEntries [ "SCHEDULED", "DEADLINE" ]
                            [ ( "SCHEDULED", "<s>" ), ( "CLOSED", "<c>" ) ]
                            Nothing
                        )
            , test "a summoned keyword already a slot ghosts nothing new" <|
                \_ ->
                    Expect.equal [ ( "SCHEDULED", "" ), ( "DEADLINE", "" ) ]
                        (Body.planEntries [ "SCHEDULED", "DEADLINE" ] [] (Just "SCHEDULED"))
            , test "a summoned keyword that is no slot lands at the end, valueless" <|
                \_ ->
                    Expect.equal [ ( "SCHEDULED", "" ), ( "DEADLINE", "" ), ( "CLOSED", "" ) ]
                        (Body.planEntries [ "SCHEDULED", "DEADLINE" ] [] (Just "CLOSED"))
            ]
        ]
