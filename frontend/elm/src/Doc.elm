port module Doc exposing (main)

{-| The materialize sheet's LEFT pane. It owns the parse, the rows, the two-axis
cursor, the grain and the delete flags; the shell keeps the keys, the edit
overlays and the writes.

The markup is the harness's and the stylesheet's: `#dlist` holds one `.de` per
stop wearing its KIND as a `d-*` class, `.dat` at point, `.dfl` on a flag,
`.dc.dc-KEY`, `.dt`/`.dl` for text, `.dg` for the unclaimed.

-}

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Json.Decode as D
import Json.Encode as E
import Body
    exposing
        ( Cell
        , Kind(..)
        , Row
        , blank
        , bodyText
        , caretIn
        , draftId
        , drafted
        , insertion
        , joinLine
        , joinWord
        , kidsOf
        , kindWord
        , ownersOf
        , placeOf
        , placeOfLine
        , rowAt
        , rowById
        , rowsFrom
        , shown
        , undrafted
        )
import Scan
import Scan exposing (Grain(..), cut, nth)



-- MODEL


type alias Link =
    { from : Int, to : Int, desc : String }


type alias Model =
    { rows : List Row
    , lines : List String
    , at : Int
    , grain : String
    , flags : List String
    , links : List Link
    , spanAt : Maybe Int
    , shift : Int
    , level : Int
    , titleAt : Maybe Int
    , child : Bool

    -- THE LINE A CURSOR IS OWED at the next fill: an insert's paragraph has no
    -- row until the RESCAN mints one.
    , landing : Maybe Int
    }


empty : Model
empty =
    Model [] [] 0 "element" [] [] Nothing 0 1 Nothing False Nothing


{-| A sibling step at the cursor's own grain. A composite is ONE stop; a leaf
steps its owner's run and never dives, since a composite sits between any two
parents' runs.
-}
step : Int -> Model -> Model
step by m =
    case rowAt m of
        Nothing ->
            m

        Just cur ->
            let
                n =
                    List.length m.rows

                grainAt i =
                    Maybe.map .grain (nth i m.rows)

                ownerAt i =
                    Maybe.andThen .owner (nth i m.rows)

                overLeaves i =
                    if i < 0 || i >= n then
                        Nothing

                    else if grainAt i == Just Leaf then
                        overLeaves (i + by)

                    else
                        Just i

                amongKin i =
                    if i < 0 || i >= n then
                        Nothing

                    else if grainAt i /= Just Leaf then
                        Nothing

                    else if ownerAt i == cur.owner then
                        Just i

                    else
                        amongKin (i + by)

                landed =
                    if cur.grain == Leaf then
                        amongKin (m.at + by)

                    else
                        overLeaves (m.at + by)
            in
            case landed of
                Nothing ->
                    m

                Just i ->
                    let
                        moved =
                            { m | at = i }
                    in
                    { moved
                        | grain =
                            if Maybe.map .grain (rowAt moved) == Just Leaf then
                                "leaf"

                            else
                                "element"
                    }


finer : Model -> ( Model, String )
finer m =
    case rowAt m of
        Nothing ->
            ( m, "" )

        Just r ->
            let
                kids =
                    if r.kind == Para then
                        kidsOf m r.id

                    else
                        0
            in
            if kids > 0 then
                -- The first child immediately follows its parent in emission order.
                ( { m | at = m.at + 1, grain = "leaf" }
                , "grain-finer (" ++ Maybe.withDefault "item" r.name ++ " 1/" ++ String.fromInt kids ++ ")"
                )

            else if r.grain == Leaf then
                ( m, "grain-finer (at the finest)" )

            else
                ( m, "grain-finer (nothing finer here)" )


broader : Model -> ( Model, String )
broader m =
    case rowAt m of
        Nothing ->
            ( m, "" )

        Just r ->
            if r.grain == Leaf then
                case Maybe.map (placeOf m) r.owner of
                    Nothing ->
                        ( { m | at = placeOf m "H", grain = "element" }
                        , "grain-broader (the headline)"
                        )

                    Just i ->
                        let
                            up =
                                Maybe.withDefault blank (nth i m.rows)

                            word =
                                case up.name of
                                    Just w ->
                                        w

                                    Nothing ->
                                        if up.grain == Leaf then
                                            "item"

                                        else
                                            kindWord up.kind
                        in
                        ( { m
                            | at = i
                            , grain =
                                if up.grain == Leaf then
                                    "leaf"

                                else
                                    "element"
                          }
                        , "grain-broader (" ++ word ++ ")"
                        )

            else if r.kind == Head then
                ( m, "grain-broader (the whole entry)" )

            else
                -- REVERSED EXPAND-REGION at its widest step: out of a leaf to its
                -- owner, out of an element to THE ENTRY'S OWN LINE.
                ( { m | at = placeOf m "H", grain = "element" }
                , "grain-broader (the headline)"
                )



-- SPANS.  OFFSETS ARE IN CHARACTERS: the title, body and properties the lens lifts out sit
-- ABOVE the paragraphs, so a body offset past the title line is displaced by
-- one constant.


charOf : Model -> Int -> Int
charOf m line =
    List.sum (List.map String.length (List.take line m.lines)) + line


elementSpan : Model -> Row -> Maybe ( Int, Int )
elementSpan m r =
    case m.spanAt of
        Nothing ->
            Nothing

        Just base ->
            case r.kind of
                Child ->
                    Nothing

                Head ->
                    Just ( base, base + charOf m 1 )

                Para ->
                    Just ( base + m.shift + charOf m r.from, base + m.shift + charOf m r.to )



-- UPDATE


type Msg
    = Fill Model
    | Clear
    | Select String
    | Restore String
    | Step Int
    | Finer
    | Broader
    | Flag String
    | Unflag String
    | ClearFlags
    | Delete (List String)
    | Edit String String
    | Draft String (Maybe Int)
    | Insert String (Maybe Int) String
    | Undraft String
    | Ignore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        Clear ->
            told empty

        Fill fresh ->
            -- The cursor comes back to the row it stood on where that row survives.
            let
                was =
                    Maybe.map .id (rowAt model)

                landed =
                    case model.landing of
                        -- A LANDING IS OWED and is spent here: the paragraph an
                        -- insert made has no id until this rescan mints one.
                        Just line ->
                            placeOfLine fresh line

                        Nothing ->
                            case was of
                                Just id ->
                                    placeOf fresh id

                                Nothing ->
                                    0
            in
            told { fresh | at = landed, landing = Nothing }

        Select id ->
            told { model | at = placeOf model id }

        Restore id ->
            told { model | at = placeOf model id }

        Step by ->
            told (step by model)

        Finer ->
            spoke (finer model)

        Broader ->
            spoke (broader model)

        Flag id ->
            -- OLDEST FIRST, the rule for every flag surface; `Listing' spells it so.
            told { model | flags = List.filter ((/=) id) model.flags ++ [ id ] }

        Unflag id ->
            told { model | flags = List.filter ((/=) id) model.flags }

        ClearFlags ->
            told { model | flags = [] }

        -- Composed HERE: a deletion cannot be rebuilt out of the model it changed.
        Delete ids ->
            let
                taken =
                    List.filter (\r -> r.kind == Para && List.member r.id ids) model.rows

                named =
                    List.filter (\r -> List.member r.id ids) model.rows
            in
            ( model
            , docTook
                (E.object
                    [ ( "taken", E.list E.string (List.map .id taken) )
                    , ( "named", E.int (List.length named) )
                    , ( "body", E.string (bodyText model (List.map .id taken)) )
                    ]
                )
            )

        Edit id written ->
            let
                write r =
                    if r.id == id then
                        { r | text = written }

                    else
                        r
            in
            composed { model | rows = List.map write model.rows }

        -- `+' DRAWS THE PARAGRAPH BEFORE IT IS WRITTEN, so the reader fills a line
        -- of their own.  The row is zero-width, which `bodyText' passes over.
        Draft id caret ->
            case ( drafted model id caret, joinWord model id caret ) of
                ( Just rows, Just word ) ->
                    -- THE WORD IS THE MODEL'S: which region the caret stands in is
                    -- `Scan''s answer, and the shell echoes it.
                    spoke
                        ( { model | rows = rows, at = placeOf { model | rows = rows } draftId }
                        , word
                        )

                _ ->
                    ( model, docSaid (E.string "nothing here takes a paragraph") )

        -- And the same row filled, which IS the write: zero-width, so the splice
        -- puts the lines in rather than replacing any.  The cursor stays put.
        Insert id caret written ->
            case ( insertion model id caret written, joinLine model id caret ) of
                ( Just rows, line ) ->
                    composed { model | rows = rows, landing = line }

                ( Nothing, _ ) ->
                    ( model, docSaid (E.string "nothing here takes a paragraph") )

        -- ESC, and an empty commit: what it leaves behind is what it found, point
        -- included — the STOP is NAMED rather than a place counted back to.
        Undraft id ->
            let
                rows =
                    undrafted model
            in
            told { model | rows = rows, at = placeOf { model | rows = rows } id }


told : Model -> ( Model, Cmd Msg )
told m =
    ( m, docState (stateJSON m) )


{-| A model whose rows have MOVED. BOTH ports, always — a `docBody' with no
`docState' would leave the shell's own copy a flush behind the file.
-}
composed : Model -> ( Model, Cmd Msg )
composed m =
    ( m
    , Cmd.batch [ docState (stateJSON m), docBody (E.string (bodyText m [])) ]
    )


spoke : ( Model, String ) -> ( Model, Cmd Msg )
spoke ( m, said ) =
    ( m, Cmd.batch [ docState (stateJSON m), docSaid (E.string said) ] )



-- PORTS


port docIn : (D.Value -> msg) -> Sub msg


port docState : E.Value -> Cmd msg


{-| What a grain key did, for the shell's echo to speak.
-}
port docSaid : E.Value -> Cmd msg


port docBody : E.Value -> Cmd msg


{-| A delete's answer: which rows it took, how many were named, and the body.
-}
port docTook : E.Value -> Cmd msg


cellJSON : Cell -> E.Value
cellJSON c =
    E.object
        [ ( "key", E.string c.key )
        , ( "val", E.string c.val )
        , ( "colour", E.string c.colour )
        ]


rowJSON : Model -> Row -> E.Value
rowJSON m r =
    E.object
        [ ( "id", E.string r.id )
        , ( "kind", E.string (kindWord r.kind) )
        , ( "grain"
          , E.string
                (case r.grain of
                    Leaf ->
                        "leaf"

                    Composite ->
                        "composite"

                    Element ->
                        "element"
                )
          )
        , ( "name", Maybe.withDefault E.null (Maybe.map E.string r.name) )
        , ( "owner", Maybe.withDefault E.null (Maybe.map E.string r.owner) )
        , ( "from", E.int r.from )
        , ( "to", E.int r.to )
        , ( "text", E.string r.text )
        , ( "index", E.int r.index )
        , ( "level", E.int r.level )
        , ( "cells", E.list cellJSON r.cells )
        , ( "span"
          , case elementSpan m r of
                Just ( a, b ) ->
                    E.list E.int [ a, b ]

                Nothing ->
                    E.null
          )
        ]


stateJSON : Model -> E.Value
stateJSON m =
    E.object
        [ ( "rows", E.list (rowJSON m) m.rows )
        , ( "at", E.int m.at )
        , ( "id", E.string (Maybe.withDefault "" (Maybe.map .id (rowAt m))) )
        , ( "grain", E.string m.grain )
        , ( "flags", E.list E.string m.flags )
        , ( "lines", E.int (List.length m.lines) )

        -- WHERE POINT GOES in the marker a draft was drawn wearing; the shell seeds
        -- its box with both and spells no org grammar of its own.
        , ( "caret"
          , E.int (caretIn (Maybe.withDefault "" (Maybe.map .text (rowById m draftId))))
          )

        -- The body as it stands, so a flush that follows no edit still has one.
        , ( "body", E.string (bodyText m []) )
        ]



-- DECODERS


cellD : D.Decoder Cell
cellD =
    D.map3 Cell
        (D.field "key" D.string)
        (D.field "val" D.string)
        (D.field "colour" D.string)


linkD : D.Decoder Link
linkD =
    D.map3 Link
        (D.field "from" D.int)
        (D.field "to" D.int)
        (D.field "desc" D.string)


{-| A fill carries the subtree the server served plus what only the shell knows:
where the span starts, how far the body is displaced, and the depth of the stars.
-}
fillD : D.Decoder Model
fillD =
    D.map8
        (\lines own headCells kids links spanAt shift level ->
            let
                seeded =
                    { empty
                        | lines = lines
                        , links = links
                        , spanAt = spanAt
                        , shift = shift
                        , level = level
                    }
            in
            { seeded | rows = rowsFrom lines own headCells kids }
        )
        (D.field "lines" (D.list D.string))
        (D.field "own" D.int)
        (D.field "cells" (D.list cellD))
        (D.field "kids" (D.list kidD))
        (D.field "links" (D.list linkD))
        (D.field "spanAt" (D.nullable D.int))
        (D.field "shift" D.int)
        (D.field "level" D.int)
        |> D.andThen
            (\m -> D.map (\t -> { m | titleAt = t }) (D.field "titleAt" (D.nullable D.int)))


kidD : D.Decoder ( Int, Int, List Cell )
kidD =
    D.map3 (\a b c -> ( a, b, c ))
        (D.field "index" D.int)
        (D.field "level" D.int)
        (D.field "cells" (D.list cellD))


{-| THE LINE THE CARET STOOD ON, and a NUMBER is the whole of what the shell
sends: WHICH REGION holds that line is asked here, and the region says both what
the new stop opens with and where it goes. ABSENT is `+' with no box open and so
no caret to read; line 0 is a line a reader stood on.
-}
caretD : D.Decoder (Maybe Int)
caretD =
    D.maybe (D.field "at" D.int)


msgD : D.Decoder Msg
msgD =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "fill" ->
                        D.map Fill fillD

                    "clear" ->
                        D.succeed Clear

                    "select" ->
                        D.map Select (D.field "id" D.string)

                    "restore" ->
                        D.map Restore (D.field "id" D.string)

                    "step" ->
                        D.map Step (D.field "by" D.int)

                    "finer" ->
                        D.succeed Finer

                    "broader" ->
                        D.succeed Broader

                    "flag" ->
                        D.map Flag (D.field "id" D.string)

                    "unflag" ->
                        D.map Unflag (D.field "id" D.string)

                    "clearFlags" ->
                        D.succeed ClearFlags

                    "delete" ->
                        D.map Delete (D.field "ids" (D.list D.string))

                    "edit" ->
                        D.map2 Edit (D.field "id" D.string) (D.field "text" D.string)

                    "insert" ->
                        D.map3 Insert
                            (D.field "id" D.string)
                            caretD
                            (D.field "text" D.string)

                    "draft" ->
                        D.map2 Draft (D.field "id" D.string) caretD

                    "undraft" ->
                        D.map Undraft (D.field "id" D.string)

                    _ ->
                        D.succeed Ignore
            )



-- VIEW


{-| ORG-CLEANED STARS: every star but the last a space, two spaces a level.
-}
stars : Model -> Int -> String
stars m level =
    String.repeat (max 0 (2 * (level - m.level))) " " ++ "* "


{-| The classes a row wears. `lvl-N` is the rung the rail is drawn on, counted in
tab stops from the list's own text; `up-K` lights the rail of the block point is
K steps inside. The stylesheet cannot do either arithmetic on a class, and
`Html.Attributes.style` cannot set a custom property in 0.19.
-}
rowClass : Model -> Int -> Row -> Int -> Bool -> String
rowClass m i r depth kin =
    (if r.id == draftId then
        "de d-draft d-"

     else
        "de d-"
    )
        ++ (case r.grain of
                Leaf ->
                    "item"

                Composite ->
                    "comp d-" ++ Maybe.withDefault "" r.name

                Element ->
                    kindWord r.kind
           )
        ++ (if i == m.at then
                " dat"

            else
                ""
           )
        ++ (if List.member r.id m.flags then
                " dfl"

            else
                ""
           )
        ++ (if depth >= 0 then
                " lvl-" ++ String.fromInt (min 11 depth)

            else
                -- ELM MOUNTS INSIDE A WRAPPER OF ITS OWN, so `#dlist > .de' names
                -- nothing and a top-level row says so itself.  NOT `d-top': the
                -- harness reads a row's KIND off its `d-' classes.
                " lvl-top"
           )
        ++ (if kin then
                " kin"

            else
                ""
           )
        ++ markOf m i r


{-| WHICH TIER A ROW'S CONNECTOR TAKES, and a row takes at most one.

  - `up-K` — the row IS point's K-th owner: THE PATH, not the level. Lighting
    every sibling of every ancestor lights whole levels and says nothing about
    the way back.
  - `mk-root` — point is a COMPOSITE and this is one of the roots it opens. A
    composite has no connector of its own, and lighting every descendant paints
    the pane and names no stop in particular.
  - `mk-held` — point is an owner of this row: what point CARRIES, in the same
    ink a shade back.

-}
markOf : Model -> Int -> Row -> String
markOf m i r =
    let
        here =
            idAtRow m m.at

        ups =
            ownersOf m r.id
    in
    if i == m.at then
        ""

    else
        case indexOfIn r.id (ownersOf m here) of
            Just k ->
                " up-" ++ String.fromInt (min 3 k)

            Nothing ->
                if r.owner == Just here && Maybe.map .grain (rowAt m) == Just Composite then
                    " mk-root"

                else if List.member here ups then
                    " mk-held"

                else
                    ""


{-| Which id point is on, or @""@ when the model holds no such row.
-}
idAtRow : Model -> Int -> String
idAtRow m i =
    Maybe.withDefault "" (Maybe.map .id (nth i m.rows))


{-| How many steps out ID sits in UPS, nearest first.
-}
indexOfIn : String -> List String -> Maybe Int
indexOfIn id ups =
    List.head
        (List.filterMap identity
            (List.indexedMap
                (\k up ->
                    if up == id then
                        Just k

                    else
                        Nothing
                )
                ups
            )
        )


{-| Text with its links marked: the shown description is the server's, the range
its span, so this page holds no bracket grammar. A link opening inside the one
before it is dropped, which rests on the scanner's non-overlap.
-}
drawText : Model -> String -> Int -> List (Html Msg)
drawText m body base =
    let
        n =
            String.length body

        inside l =
            l.from >= base && l.to <= base + n

        go links seen out =
            case links of
                [] ->
                    if seen == 0 then
                        [ text body ]

                    else if seen < n then
                        out ++ [ span [ class "dt" ] [ text (String.dropLeft seen body) ] ]

                    else
                        out

                l :: rest ->
                    let
                        a =
                            l.from - base

                        b =
                            l.to - base
                    in
                    if a < seen then
                        go rest seen out

                    else
                        go rest
                            b
                            (out
                                ++ (if a > seen then
                                        [ span [ class "dt" ] [ text (String.slice seen a body) ] ]

                                    else
                                        []
                                   )
                                ++ [ span [ class "dl" ] [ text l.desc ] ]
                            )
    in
    go (List.filter inside m.links) 0 []


{-| A row's own line. THE MARKER ORG WROTE IS ITS OWN SPAN, so point can light it
the way a headline lights its stars; `drawText` slices by ABSOLUTE offsets, so the
prefix moves the base with it rather than being spliced out of the middle.
-}
viewPara : Model -> Row -> Html Msg
viewPara m r =
    let
        k =
            markerLen m r

        rest =
            String.dropLeft k r.text

        mark =
            if k > 0 then
                [ span [ class "dm" ] [ text (String.left k r.text) ] ]

            else
                []
    in
    div [ class "dp" ]
        (mark
            ++ (case elementSpan m r of
                    Just ( a, _ ) ->
                        drawText m rest (a + k)

                    Nothing ->
                        [ text rest ]
               )
        )


{-| How many characters of a leaf's own line org spent on its indent and its
bullet -- `-', `+', `*', `1.' or `1)' -- and nothing when the row is not one.
-}
markerLen : Model -> Row -> Int
markerLen m r =
    if r.grain /= Leaf then
        0

    else
        case Maybe.andThen Scan.listOpener (nth r.from m.lines) of
            Just o ->
                o.indent + String.length o.bullet

            Nothing ->
                0


viewCells : Model -> Int -> Row -> List (Html Msg)
viewCells m i r =
    span [ class "ds" ]
        [ text
            (stars m
                (if r.kind == Child then
                    r.level

                 else
                    m.level
                )
            )
        ]
        :: List.indexedMap
            (\j c ->
                span
                    [ class
                        ("dc dc-" ++ c.key)
                    , style "color" c.colour
                    ]
                    (case ( c.key, r.kind, m.titleAt ) of
                        ( "title", Head, Just t ) ->
                            drawText m c.val t

                        _ ->
                            [ text c.val ]
                    )
            )
            (shown r)


{-| ONE OWNER PER BYTE: a composite is drawn once with its leaves inside it, and
what no rung claims is drawn INERT (`dg`).
-}
viewKids : Model -> Row -> Int -> Int -> Int -> ( List (Html Msg), Int )
viewKids m parent from at0 depth =
    let
        n =
            List.length m.rows

        rowN j =
            nth j m.rows

        go j mark out =
            case rowN j of
                Just kid ->
                    if kid.kind == Para && kid.owner == Just parent.id then
                        let
                            gap =
                                if kid.from > mark then
                                    [ div [ class "dg" ] [ text (cut m.lines mark kid.from) ] ]

                                else
                                    []

                            under =
                                case rowN (j + 1) of
                                    Just next ->
                                        next.owner == Just kid.id

                                    Nothing ->
                                        False

                            ( inner, jNext ) =
                                if under then
                                    let
                                        headAt =
                                            Maybe.withDefault kid.from (Maybe.map .from (rowN (j + 1)))

                                        own =
                                            if headAt > kid.from then
                                                [ viewPara m
                                                    { kid | to = headAt, text = cut m.lines kid.from headAt }
                                                ]

                                            else
                                                []

                                        ( deeper, jj ) =
                                            viewKids m kid (j + 1) headAt (depth + 1)
                                    in
                                    ( own ++ deeper, jj )

                                else
                                    ( [ viewPara m kid ], j + 1 )

                            -- A LATER SIBLING IS WHAT CARRIES THE BRANCH ON past
                            -- this row: the next row after this one's subtree,
                            -- owned by the same parent.
                            kin =
                                case rowN jNext of
                                    Just next ->
                                        next.kind == Para && next.owner == Just parent.id

                                    Nothing ->
                                        False
                        in
                        go jNext
                            kid.to
                            (out ++ gap ++ [ div [ class (rowClass m j kid depth kin) ] inner ])

                    else
                        ( tail mark out, j )

                Nothing ->
                    ( tail mark out, j )

        tail mark out =
            if mark < parent.to then
                out ++ [ div [ class "dg" ] [ text (cut m.lines mark parent.to) ] ]

            else
                out
    in
    go from
        (if at0 < 0 then
            parent.from

         else
            at0
        )
        []


view : Model -> Html Msg
view m =
    let
        n =
            List.length m.rows

        go i out =
            if i >= n then
                out

            else
                let
                    r =
                        Maybe.withDefault blank (nth i m.rows)
                in
                if r.grain == Composite then
                    let
                        ( inner, j ) =
                            viewKids m r (i + 1) -1 0
                    in
                    go j (out ++ [ div [ class (rowClass m i r -1 False) ] inner ])

                else if r.kind == Para then
                    go (i + 1) (out ++ [ div [ class (rowClass m i r -1 False) ] [ viewPara m r ] ])

                else
                    go (i + 1) (out ++ [ div [ class (rowClass m i r -1 False) ] (viewCells m i r) ])
    in
    div [] (go 0 [])



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( empty, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \_ -> docIn (\v -> Result.withDefault Ignore (D.decodeValue msgD v))
        }
