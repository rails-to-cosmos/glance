port module Doc exposing (main)

{-| The materialize sheet's LEFT pane: a subtree's headline line, the body's own
structure, and the child headlines under it.

It owns the parse, the rows, the two-axis cursor, the grain and the delete
flags, and it draws them. The shell keeps the keys, the edit overlays and the
writes, and mirrors the state pushed back here.

The markup is the one the harness and the stylesheet read: `#dlist` holds one
`.de` per stop, wearing its KIND as a `d-*` class, `.dat` where point is, `.dfl`
where a flag is, cells as `.dc.dc-KEY` with `.don` on the one under point, text
as `.dt` and a link's shown text as `.dl`, and whatever no rung claims as `.dg`.
-}

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type Grain
    = Element
    | Composite
    | Leaf


type Kind
    = Head
    | Para
    | Child


type alias Cell =
    { key : String, val : String }


type alias Link =
    { from : Int, to : Int, desc : String }


type alias Row =
    { id : String
    , kind : Kind
    , grain : Grain
    , name : Maybe String
    , owner : Maybe String
    , from : Int
    , to : Int
    , text : String
    , was : String
    , cells : List Cell
    , index : Int
    , level : Int
    }


blank : Row
blank =
    Row "" Para Element Nothing Nothing 0 0 "" "" [] 0 1


type alias Model =
    { rows : List Row
    , lines : List String
    , at : Int
    , col : Maybe Int
    , grain : String
    , flags : List String
    , links : List Link
    , spanAt : Maybe Int
    , shift : Int
    , level : Int
    , titleAt : Maybe Int
    , child : Bool
    }


empty : Model
empty =
    Model [] [] 0 Nothing "element" [] [] Nothing 0 1 Nothing False



-- THE STRUCTURE SCANNER
--
-- A port of the shell's own, rule for rule. The openers are the corpus's: `-`,
-- `1.`/`1)`, `+`, and an INDENTED `*` — a `* ` at column 1 is a headline.


type alias Opener =
    { indent : Int }


listOpener : String -> Maybe Opener
listOpener line =
    let
        spaces =
            String.length line - String.length (String.trimLeft line)

        rest =
            String.dropLeft spaces line

        bulletAt =
            if String.startsWith "- " rest || rest == "-" then
                Just "-"

            else if String.startsWith "+ " rest || rest == "+" then
                Just "+"

            else if String.startsWith "* " rest || rest == "*" then
                Just "*"

            else
                numberedAt rest
    in
    case bulletAt of
        Nothing ->
            Nothing

        -- A `* ' at COLUMN 1 is a headline rather than an item.
        Just "*" ->
            if spaces == 0 then
                Nothing

            else
                Just (Opener spaces)

        Just _ ->
            Just (Opener spaces)


{-| `1.` or `1)` followed by a space or the line's end.
-}
numberedAt : String -> Maybe String
numberedAt rest =
    let
        digits =
            String.toList rest |> takeWhileList Char.isDigit |> String.fromList

        after =
            String.dropLeft (String.length digits) rest
    in
    if String.isEmpty digits then
        Nothing

    else if String.startsWith ". " after || String.startsWith ") " after
            || after == "." || after == ")" then
        Just digits

    else
        Nothing


takeWhileList : (a -> Bool) -> List a -> List a
takeWhileList f xs =
    case xs of
        [] ->
            []

        y :: rest ->
            if f y then
                y :: takeWhileList f rest

            else
                []


blockName : String -> Maybe String
blockName line =
    let
        low =
            String.toLower (String.trimLeft line)
    in
    if String.startsWith "#+begin_" low then
        case String.words (String.dropLeft 8 low) of
            w :: _ ->
                if String.isEmpty w then
                    Nothing

                else
                    Just w

            [] ->
                Nothing

    else
        Nothing


closes : String -> String -> Bool
closes name line =
    String.toLower (String.trim line) == "#+end_" ++ name


isTable : String -> Bool
isTable line =
    String.startsWith "|" (String.trimLeft line)


isBlank : String -> Bool
isBlank line =
    String.trim line == ""


{-| A line that RIDES INSIDE the item above it: an opener of its own, or an
indented continuation.
-}
rides : String -> Bool
rides line =
    listOpener line /= Nothing || String.startsWith " " line || String.startsWith "\t" line


at : Int -> List String -> String
at i xs =
    Maybe.withDefault "" (List.head (List.drop i xs))


cut : List String -> Int -> Int -> String
cut lines a b =
    String.join "\n" (List.take (b - a) (List.drop a lines))


{-| Where the block opened at I closes, or -1.
-}
blockRun : List String -> Int -> Int -> String -> Int
blockRun lines i end name =
    let
        go j =
            if j >= end then
                -1

            else if closes name (at j lines) then
                j + 1

            else
                go (j + 1)
    in
    go (i + 1)


type alias Run =
    { to : Int, items : List ( Int, Int ) }


{-| ONE BLANK LINE STAYS IN — org's rule. Two close the list, as does a blank
with something that does not ride under it.
-}
listRun : List String -> Int -> Int -> Run
listRun lines i end =
    let
        base =
            case listOpener (at i lines) of
                Just o ->
                    o.indent

                Nothing ->
                    0

        blanksFrom j =
            if j < end && isBlank (at j lines) then
                blanksFrom (j + 1)

            else
                j

        go j from last items =
            if j >= end then
                Run last (close from last items)

            else if isBlank (at j lines) then
                let
                    k =
                        blanksFrom j
                in
                if k - j > 1 || k >= end || not (rides (at k lines)) then
                    Run last (close from last items)

                else
                    go k from last items

            else
                case listOpener (at j lines) of
                    Just o ->
                        if o.indent <= base then
                            go (j + 1) j (j + 1) (close from last items)

                        else
                            go (j + 1) from (j + 1) items

                    Nothing ->
                        if rides (at j lines) then
                            go (j + 1) from (j + 1) items

                        else
                            Run last (close from last items)

        close from last items =
            if from == -1 then
                items

            else
                items ++ [ ( from, last ) ]
    in
    go i -1 i []


{-| The non-blank runs between A and B, each as its own leaf.
-}
runsIn : List String -> Int -> Int -> List ( Int, Int )
runsIn lines a b =
    let
        go i from out =
            if i > b then
                out

            else if i == b || isBlank (at i lines) then
                if from == -1 then
                    go (i + 1) -1 out

                else
                    go (i + 1) -1 (out ++ [ ( from, i ) ])

            else if from == -1 then
                go (i + 1) i out

            else
                go (i + 1) from out
    in
    go a -1 []


type alias Block =
    { from : Int, to : Int, grain : Grain, name : Maybe String, up : Maybe Int }


{-| The body's structure, emitted INLINE as `[whole, leaf1..leafN]` — the walk
reads that order. `up` indexes OUT, at the leaf's IMMEDIATE owner, so the grain
is a ladder. OWN is the server's `ownLines`, which is what keeps a child's bytes
out of the body's paragraphs.
-}
blocksIn : List String -> Int -> List Block
blocksIn lines own =
    let
        end =
            max 0 (min own (List.length lines))

        -- A list item, plus any run nested deeper inside it.
        pushItem from to up out =
            let
                here =
                    List.length out

                base =
                    case listOpener (at from lines) of
                        Just o ->
                            o.indent

                        Nothing ->
                            0

                nested n acc =
                    if n >= to then
                        acc

                    else
                        case listOpener (at n lines) of
                            Just o ->
                                if o.indent > base then
                                    let
                                        run =
                                            listRun lines n to

                                        deeper =
                                            List.foldl
                                                (\( a, b ) got -> pushItem a b (Just here) got)
                                                acc
                                                run.items
                                    in
                                    nested (max (n + 1) run.to) deeper

                                else
                                    nested (n + 1) acc

                            Nothing ->
                                nested (n + 1) acc
            in
            nested (from + 1) (out ++ [ Block from to Leaf Nothing up ])

        whole a b name leaves out =
            let
                here =
                    List.length out
            in
            List.foldl (\( p, q ) got -> got ++ [ Block p q Leaf Nothing (Just here) ])
                (out ++ [ Block a b Composite (Just name) Nothing ])
                leaves

        -- Where a plain paragraph run ends: the next blank line or structure.
        paraEnd j =
            if j >= end then
                j

            else if isBlank (at j lines)
                || listOpener (at j lines) /= Nothing
                || blockName (at j lines) /= Nothing
                || isTable (at j lines) then
                j

            else
                paraEnd (j + 1)

        tableEnd j =
            if j < end && isTable (at j lines) then
                tableEnd (j + 1)

            else
                j

        go i out =
            if i >= end then
                out

            else if isBlank (at i lines) then
                go (i + 1) out

            else
                case blockName (at i lines) of
                    Just name ->
                        let
                            shut =
                                blockRun lines i end name
                        in
                        if shut /= -1 then
                            go shut (whole i shut name (runsIn lines (i + 1) (shut - 1)) out)

                        else
                            plain i out

                    Nothing ->
                        if isTable (at i lines) then
                            let
                                j =
                                    tableEnd i
                            in
                            go j (whole i j "table" (List.map (\n -> ( n, n + 1 )) (List.range i (j - 1))) out)

                        else if listOpener (at i lines) /= Nothing then
                            let
                                run =
                                    listRun lines i end

                                here =
                                    List.length out

                                opened =
                                    out ++ [ Block i run.to Composite (Just "list") Nothing ]
                            in
                            go (max (i + 1) run.to)
                                (List.foldl (\( a, b ) got -> pushItem a b (Just here) got) opened run.items)

                        else
                            plain i out

        plain i out =
            let
                j =
                    paraEnd (i + 1)
            in
            go j (out ++ [ Block i j Element Nothing Nothing ])
    in
    go 1 []



-- THE PARSE INTO ROWS


rowsFrom : Model -> List String -> Int -> List Cell -> List ( Int, Int, List Cell ) -> List Row
rowsFrom m lines own headCells kids =
    let
        head =
            { blank | id = "H", kind = Head, grain = Element, cells = headCells }

        blocks =
            blocksIn lines own

        ids =
            List.indexedMap (\i _ -> "B" ++ String.fromInt i) blocks

        idAt k =
            Maybe.withDefault "" (List.head (List.drop k ids))

        body =
            List.map2
                (\b i ->
                    { blank
                        | id = i
                        , kind = Para
                        , grain = b.grain
                        , name = b.name
                        , owner = Maybe.map idAt b.up
                        , from = b.from
                        , to = b.to
                        , text = cut lines b.from b.to
                        , was = cut lines b.from b.to
                    }
                )
                blocks
                ids

        child ( index, level, cells ) =
            { blank
                | id = "C" ++ String.fromInt index
                , kind = Child
                , grain = Element
                , index = index
                , level = level
                , cells = cells
            }
    in
    (head :: body) ++ List.map child kids



-- THE SPLICE
--
-- ONE GRAIN SPEAKS FOR A RANGE: a composite and its leaves cover the same
-- lines, so a moved or going ancestor silences every rung under it. Bottom-up,
-- so an earlier range is never moved by a later splice.


ownersOf : Model -> String -> List String
ownersOf m id =
    case Maybe.andThen .owner (rowById m id) of
        Nothing ->
            []

        Just up ->
            up :: ownersOf m up


bodyText : Model -> List String -> String
bodyText m gone =
    let
        moved r =
            r.kind == Para && (List.member r.id gone || r.text /= r.was)

        spoken =
            List.map .id (List.filter moved m.rows)

        silenced r =
            List.any (\o -> List.member o spoken) (ownersOf m r.id)

        paras =
            List.reverse (List.filter (\r -> r.kind == Para && not (silenced r)) m.rows)

        splice r out =
            if List.member r.id gone then
                let
                    -- A paragraph taken out takes the blank line under it too.
                    spare =
                        if r.to < List.length out - 1 && isBlank (at r.to out) then
                            1

                        else
                            0
                in
                List.take r.from out ++ List.drop (r.to + spare) out

            else if r.text /= r.was then
                List.take r.from out ++ String.split "\n" r.text ++ List.drop r.to out

            else
                out
    in
    String.join "\n" (List.foldl splice m.lines paras)



-- CURSOR AND GRAIN


rowById : Model -> String -> Maybe Row
rowById m id =
    List.head (List.filter (\r -> r.id == id) m.rows)


rowAt : Model -> Maybe Row
rowAt m =
    List.head (List.drop m.at m.rows)


placeOf : Model -> String -> Int
placeOf m id =
    List.indexedMap (\i r -> ( i, r.id )) m.rows
        |> List.filter (\( _, rid ) -> rid == id)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault m.at


shown : Row -> List Cell
shown r =
    List.filter (\c -> c.val /= "") r.cells


cellCount : Maybe Row -> Int
cellCount mr =
    case mr of
        Just r ->
            if r.kind == Head || r.kind == Child then
                List.length (shown r)

            else
                0

        Nothing ->
            0


kidsOf : Model -> String -> Int
kidsOf m id =
    List.length (List.filter (\r -> r.kind == Para && r.owner == Just id) m.rows)


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
                    Maybe.map .grain (List.head (List.drop i m.rows))

                ownerAt i =
                    Maybe.andThen .owner (List.head (List.drop i m.rows))

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

                        col =
                            if cellCount (rowAt moved) == 0 then
                                Nothing

                            else
                                m.col
                    in
                    { moved
                        | col = col
                        , grain =
                            if col /= Nothing then
                                "cell"

                            else if Maybe.map .grain (rowAt moved) == Just Leaf then
                                "leaf"

                            else
                                "element"
                    }


{-| The column walk. Out of range on either side is the whole-row look, which is
what makes `f` at the last cell leave the cells rather than stop on them.
-}
moveCol : Int -> Model -> ( Model, String )
moveCol by m =
    let
        n =
            cellCount (rowAt m)
    in
    if n == 0 then
        ( m, "next-column (no cells in this element)" )

    else
        let
            want =
                case m.col of
                    Nothing ->
                        if by > 0 then
                            0

                        else
                            n - 1

                    Just c ->
                        c + by

            col =
                if want < 0 || want >= n then
                    Nothing

                else
                    Just want

            named =
                case ( col, rowAt m ) of
                    ( Just c, Just r ) ->
                        Maybe.withDefault "" (Maybe.map .key (List.head (List.drop c (shown r))))

                    _ ->
                        "element mode"
        in
        ( { m | col = col, grain = if col == Nothing then "element" else "cell" }
        , "next-column (" ++ named ++ ")"
        )


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

            else if cellCount (Just r) > 0 then
                moveCol 1 m

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
            if m.col /= Nothing then
                ( { m | col = Nothing, grain = "element" }, "grain-broader (element)" )

            else if r.grain == Leaf then
                case Maybe.map (placeOf m) r.owner of
                    Nothing ->
                        ( m, "grain-broader (at the element grain)" )

                    Just i ->
                        let
                            up =
                                Maybe.withDefault blank (List.head (List.drop i m.rows))

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
                        ( { m | at = i, grain = if up.grain == Leaf then "leaf" else "element" }
                        , "grain-broader (" ++ word ++ ")"
                        )

            else
                ( m, "grain-broader (at the element grain)" )


kindWord : Kind -> String
kindWord k =
    case k of
        Head ->
            "head"

        Para ->
            "para"

        Child ->
            "child"



-- SPANS
--
-- OFFSETS ARE IN CHARACTERS. The three regions the lens lifts out sit ABOVE the
-- paragraphs, so a body offset past the title line is displaced by one constant.


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
    | Restore String (Maybe Int)
    | Step Int
    | Finer
    | Broader
    | Col Int
    | Flag String
    | Unflag String
    | ClearFlags
    | Delete (List String)
    | Edit String String
    | Ignore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        Clear ->
            told empty

        Fill fresh ->
            -- The cursor comes back to the row it stood on where that row
            -- survives, which is what makes a re-read leave point alone.
            let
                was =
                    Maybe.map .id (rowAt model)

                landed =
                    case was of
                        Just id ->
                            placeOf fresh id

                        Nothing ->
                            0
            in
            told { fresh | at = landed, col = Nothing }

        Select id ->
            told { model | at = placeOf model id }

        Restore id col ->
            told { model | at = placeOf model id, col = col }

        Step by ->
            told (step by model)

        Finer ->
            spoke (finer model)

        Broader ->
            spoke (broader model)

        Col by ->
            spoke (moveCol by model)

        Flag id ->
            told { model | flags = id :: List.filter ((/=) id) model.flags }

        Unflag id ->
            told { model | flags = List.filter ((/=) id) model.flags }

        ClearFlags ->
            told { model | flags = [] }

        -- The body a write sends is composed HERE, since a deletion cannot be
        -- rebuilt out of the model it just changed.
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

                after =
                    { model | rows = List.map write model.rows }
            in
            ( after
            , Cmd.batch
                [ docState (stateJSON after)
                , docBody (E.string (bodyText after []))
                ]
            )


told : Model -> ( Model, Cmd Msg )
told m =
    ( m, docState (stateJSON m) )


spoke : ( Model, String ) -> ( Model, Cmd Msg )
spoke ( m, said ) =
    ( m, Cmd.batch [ docState (stateJSON m), docSaid (E.string said) ] )



-- PORTS


port docIn : (D.Value -> msg) -> Sub msg


port docState : E.Value -> Cmd msg


{-| What a grain key did, for the shell's echo to speak.
-}
port docSaid : E.Value -> Cmd msg


{-| The body a paragraph edit composed, for the write that follows it.
-}
port docBody : E.Value -> Cmd msg


{-| A delete's answer: which rows it took, how many were named, and the body.
-}
port docTook : E.Value -> Cmd msg


cellJSON : Cell -> E.Value
cellJSON c =
    E.object [ ( "key", E.string c.key ), ( "val", E.string c.val ) ]


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
        , ( "col", Maybe.withDefault E.null (Maybe.map E.int m.col) )
        , ( "grain", E.string m.grain )
        , ( "flags", E.list E.string m.flags )
        , ( "lines", E.int (List.length m.lines) )
        -- The body as it stands, so a flush that follows no edit still has one.
        , ( "body", E.string (bodyText m []) )
        ]



-- DECODERS


cellD : D.Decoder Cell
cellD =
    D.map2 Cell (D.field "key" D.string) (D.field "val" D.string)


linkD : D.Decoder Link
linkD =
    D.map3 Link
        (D.field "from" D.int)
        (D.field "to" D.int)
        (D.field "desc" D.string)


{-| A fill carries the subtree the server served plus what only the shell knows:
where the entry's span starts, how far the body is displaced inside it, and the
depth the stars are drawn relative to.
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
            { seeded | rows = rowsFrom seeded lines own headCells kids }
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
                        D.map2 Restore (D.field "id" D.string) (D.field "col" (D.nullable D.int))

                    "step" ->
                        D.map Step (D.field "by" D.int)

                    "finer" ->
                        D.succeed Finer

                    "broader" ->
                        D.succeed Broader

                    "col" ->
                        D.map Col (D.field "by" D.int)

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

                    _ ->
                        D.succeed Ignore
            )



-- VIEW


{-| ORG-CLEANED STARS: every star but the last a space, two spaces a level.
-}
stars : Model -> Int -> String
stars m level =
    String.repeat (max 0 (2 * (level - m.level))) " " ++ "* "


rowClass : Model -> Int -> Row -> String
rowClass m i r =
    "de d-"
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


viewPara : Model -> Row -> Html Msg
viewPara m r =
    div [ class "dp" ]
        (case elementSpan m r of
            Just ( a, _ ) ->
                drawText m r.text a

            Nothing ->
                [ text r.text ]
        )


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
                        ("dc dc-"
                            ++ c.key
                            ++ (if i == m.at && Just j == m.col then
                                    " don"

                                else
                                    ""
                               )
                        )
                    , style "color"
                        (if c.key == "state" then
                            "var(--g-state-" ++ c.val ++ ", inherit)"

                         else
                            ""
                        )
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
viewKids : Model -> Row -> Int -> Int -> ( List (Html Msg), Int )
viewKids m parent from at0 =
    let
        n =
            List.length m.rows

        rowN j =
            List.head (List.drop j m.rows)

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
                                            viewKids m kid (j + 1) headAt
                                    in
                                    ( own ++ deeper, jj )

                                else
                                    ( [ viewPara m kid ], j + 1 )
                        in
                        go jNext
                            kid.to
                            (out ++ gap ++ [ div [ class (rowClass m j kid) ] inner ])

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
                        Maybe.withDefault blank (List.head (List.drop i m.rows))
                in
                if r.grain == Composite then
                    let
                        ( inner, j ) =
                            viewKids m r (i + 1) -1
                    in
                    go j (out ++ [ div [ class (rowClass m i r) ] inner ])

                else if r.kind == Para then
                    go (i + 1) (out ++ [ div [ class (rowClass m i r) ] [ viewPara m r ] ])

                else
                    go (i + 1) (out ++ [ div [ class (rowClass m i r) ] (viewCells m i r) ])
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
