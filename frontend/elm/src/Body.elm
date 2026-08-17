module Body exposing
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
    , markerFor
    , placeOf
    , placeOfLine
    , rowAt
    , rowById
    , rowsFrom
    , shown
    , undrafted
    )

{-| WHAT THE PANE MAKES OF IT: the rows a body becomes, the splice that
composes one back, the markers a new line opens with, and the readings a
cursor is moved by.  Reads `Scan' for the structure; nothing here reads back.
-}

import Array exposing (Array)
import Scan exposing (Grain(..), Opener, Region, RegionKind(..), at, blankAt,
        closers,
        blocksIn, closerAt, cut, indentOf, isTable, listOpener, nth, numberAt,
        regionAt, takeWhileList)


type Kind
    = Head
    | Para
    | Child


type alias Cell =
    { key : String, val : String, colour : String }


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

    -- The SPLICE is what writes the separator, so the draft carries this.
    , alone : Bool
    }


blank : Row
blank =
    Row "" Para Element Nothing Nothing 0 0 "" "" [] 0 1 False

-- THE PARSE INTO ROWS


rowsFrom : List String -> Int -> List Cell -> List ( Int, Int, List Cell ) -> List Row
rowsFrom lines own headCells kids =
    let
        head =
            { blank | id = "H", kind = Head, grain = Element, cells = headCells }

        blocks =
            blocksIn (Array.fromList lines) own

        ids =
            List.indexedMap (\i _ -> "B" ++ String.fromInt i) blocks

        idAt k =
            Maybe.withDefault "" (nth k ids)

        body =
            List.map2
                (\b i ->
                    let
                        held =
                            cut lines b.from b.to
                    in
                    { blank
                        | id = i
                        , kind = Para
                        , grain = b.grain
                        , name = b.name
                        , owner = Maybe.map idAt b.up
                        , from = b.from
                        , to = b.to
                        , text = held
                        , was = held
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
        owned r =
            let
                stop =
                    ownEnd body r
            in
            if stop == r.to then
                r

            else
                { r | text = cut lines r.from stop, was = cut lines r.from stop }
    in
    (head :: List.map owned body) ++ List.map child kids



-- THE SPLICE
-- ONE GRAIN SPEAKS FOR A RANGE; bottom-up, so a later splice moves no earlier one.


{-| Where a row's OWN line ends: its first child's start, else its own end. A
nested item is DRAWN inside its parent and would be WRITTEN as part of it, so
this is the one number saying which lines are the row's to show and to replace.
-}
ownEnd : List Row -> Row -> Int
ownEnd rows r =
    if r.grain == Composite then
        -- A COMPOSITE IS THE WHOLE THING: the list is one stop, so editing it
        -- rewrites the list and its leaves are silenced under it.
        r.to

    else
        case List.filter (\k -> k.owner == Just r.id) rows of
            kid :: _ ->
                kid.from

            [] ->
                r.to


ownersOf : { a | rows : List Row } -> String -> List String
ownersOf m id =
    case Maybe.andThen .owner (rowById m id) of
        Nothing ->
            []

        Just up ->
            up :: ownersOf m up


{-| The leading spaces of a text's FIRST line, which is the line a row owns.
-}
indentIn : String -> Int
indentIn text =
    String.length
        (Scan.indentOf (Maybe.withDefault "" (List.head (String.split "\n" text))))


{-| Move LINE by N spaces, never past column zero.
-}
nudge : Int -> String -> String
nudge n line =
    if n > 0 then
        String.repeat n " " ++ line

    else if n < 0 then
        String.dropLeft (min -n (String.length (Scan.indentOf line))) line

    else
        line


bodyText : { a | rows : List Row, lines : List String } -> List String -> String
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
                        if r.to < List.length out - 1 && blankAt r.to out then
                            1

                        else
                            0
                in
                List.take r.from out ++ List.drop (r.to + spare) out

            else if r.text /= r.was then
                let
                    typed =
                        String.split "\n" r.text

                    -- A COMMIT CLOSES WHAT THE TYPING OPENED: an opener with no
                    -- closer gets one, innermost first.  Balanced text gains none.
                    written =
                        typed ++ closers typed


                    -- THE SUBTREE RIDES ALONG.  A row's own line is spliced over
                    -- its own extent, so re-indenting it without moving what hangs
                    -- off it would reparent the lot to whatever stands above.
                    cut =
                        ownEnd m.rows r

                    shift =
                        indentIn r.text - indentIn r.was

                    under =
                        List.map (nudge shift)
                            (List.take (r.to - cut) (List.drop cut out))
                in
                List.take r.from out
                    ++ (if r.alone then
                            -- The blanks that keep a paragraph one are the
                            -- SPLICE's: a zero-width range ADDS lines.
                            apart out r.from written

                        else
                            written
                       )
                    ++ under
                    ++ List.drop r.to out

            else
                out
    in
    String.join "\n" (List.foldl splice m.lines paras)



-- THE MARKERS


{-| ONE ANSWER PER REGION: what a new line inside it opens with, org's own in
each case — a checkbox item's box arrives EMPTY, a table row aligned.
-}
markerFor : Array String -> Region -> String
markerFor lines reg =
    let
        indent =
            indentOf (at reg.from lines)
    in
    case reg.kind of
        Plain ->
            ""

        Item ->
            itemMarker lines reg.from

        Table ->
            indent ++ tableRow lines reg.from reg.to

        Block ->
            indent

        Drawer ->
            indent


itemMarker : Array String -> Int -> String
itemMarker lines from =
    let
        line =
            at from lines
    in
    case listOpener line of
        Nothing ->
            indentOf line

        Just o ->
            String.left o.indent line
                ++ nextBullet line o
                ++ boxAfter (String.dropLeft (o.indent + String.length o.bullet) line)


nextBullet : String -> Opener -> String
nextBullet line o =
    let
        digits =
            String.fromList (takeWhileList Char.isDigit (String.toList o.bullet))
    in
    case ( String.isEmpty digits, numberAt line ) of
        ( False, Just n ) ->
            String.fromInt (n + 1) ++ String.dropLeft (String.length digits) o.bullet

        _ ->
            o.bullet


boxAfter : String -> String
boxAfter after =
    if List.member (String.left 3 after) [ "[ ]", "[X]", "[x]", "[-]" ] then
        "[ ] "

    else
        ""


tableRow : Array String -> Int -> Int -> String
tableRow lines from to =
    Array.toList (Array.slice from to lines)
        |> List.filter (not << isRule)
        |> List.map (List.map String.length << tableCells)
        |> List.foldl widest []
        |> List.map (\w -> String.repeat w " " ++ "|")
        |> String.concat
        |> (++) "|"


isRule : String -> Bool
isRule line =
    String.startsWith "|-" (String.trimLeft line)


tableCells : String -> List String
tableCells line =
    let
        cells =
            List.drop 1 (String.split "|" (String.trim line))

        n =
            List.length cells
    in
    if nth (n - 1) cells == Just "" then
        List.take (n - 1) cells

    else
        cells


{-| WHERE POINT GOES IN A MARKER: its end, except a TABLE ROW — typing past the
closing pipe would open a column org's align then keeps, so point goes inside.
-}
caretIn : String -> Int
caretIn marker =
    let
        open =
            String.length (indentOf marker) + 1

        firstCell =
            open + String.length (Maybe.withDefault "" (List.head (tableCells marker)))
    in
    if isTable marker then
        clamp open firstCell (open + 1)

    else
        String.length marker


widest : List Int -> List Int -> List Int
widest new acc =
    case ( new, acc ) of
        ( [], rest ) ->
            rest

        ( rest, [] ) ->
            rest

        ( a :: more, b :: rest ) ->
            max a b :: widest more rest



-- THE INSERT — DRAWN before it is written: a ZERO-WIDTH row wearing the marker
-- alone, which `bodyText' passes over, its text not having moved off its `was'.


draftId : String
draftId =
    "D"


{-| WHERE a sibling joins: the row it goes in under, the BODY LINE it takes, the
MARKER it opens wearing, whether the splice owes it blank lines, and THE WORD.
-}
type alias Join =
    { under : String
    , line : Int
    , marker : String
    , owner : Maybe String
    , alone : Bool
    , word : String
    }


joinWord : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe String
joinWord m id caret =
    Maybe.map .word (joinAt m id caret)


pastWord : Row -> String
pastWord top =
    case top.name of
        Just name ->
            "after the " ++ name

        Nothing ->
            if top.grain == Composite then
                "after the block"

            else
                "after this paragraph"


regionWord : RegionKind -> String
regionWord kind =
    case kind of
        Item ->
            "an item at this level"

        Table ->
            "a row in this table"

        Block ->
            "a line in this block"

        Drawer ->
            "a line in this drawer"

        Plain ->
            "a line here"


{-| WHERE `+' joins. A CARET is a line INSIDE the stop, where `S-RET' was
pressed; without one there is nothing to be inside, so it is a sibling of it.
-}
joinAt : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe Join
joinAt m id caret =
    case rowById m id of
        Nothing ->
            Nothing

        Just r ->
            case r.kind of
                Child ->
                    Nothing

                Head ->
                    Just (Join r.id 1 "" Nothing True "at the top")

                Para ->
                    Just
                        (case caret of
                            Just off ->
                                inside m r (caretLine r off)

                            Nothing ->
                                sibling m r
                        )


{-| `+' WITH NO BOX OPEN NAMES NO LINE, so THE GRAIN IS THE SELECTOR: a list
LEAF takes a sibling of the stop, everything else rides past its structure.
-}
sibling : { a | rows : List Row, lines : List String } -> Row -> Join
sibling m r =
    let
        top =
            outermost m r
    in
    if top.name == Just "list" && r.grain == Leaf then
        Join r.id
            r.to
            (itemMarker (Array.fromList m.lines) r.from)
            r.owner
            False
            (regionWord Item)

    else
        Join top.id top.to "" Nothing True (pastWord top)


{-| THE REGION HOLDING THE CARET'S LINE ANSWERS BOTH HALVES: its marker is what
the new line opens with, its interior is where that line goes. A CLOSING LINE
asks for what comes AFTER; a TABLE has none, which is how a table is built.
-}
inside : { a | rows : List Row, lines : List String } -> Row -> Int -> Join
inside m r line =
    let
        top =
            outermost m r

        lines =
            Array.fromList m.lines

        reg =
            regionAt lines top.from top.to line
    in
    if reg.kind == Plain || closerAt reg line then
        Join top.id reg.to "" Nothing True (pastWord top)

    else
        anchored m r (line + 1) (markerFor lines reg) (regionWord reg.kind)


{-| THE OWNER IS THE ANCHORED LINE'S. `Doc.viewKids' walks a composite's kids
while their owner is its own, so a disagreeing owner draws leaves twice.
-}
anchored : { a | rows : List Row } -> Row -> Int -> String -> String -> Join
anchored m r line marker word =
    let
        host =
            holding m r (line - 1)
    in
    Join r.id
        line
        marker
        (if line >= host.to then
            host.owner

         else
            Just host.id
        )
        False
        word


caretLine : Row -> Int -> Int
caretLine r off =
    r.from + clamp 0 (max 0 (r.to - r.from - 1)) off


holding : { a | rows : List Row } -> Row -> Int -> Row
holding m r line =
    List.foldl
        (\s best ->
            if s.from >= best.from then
                s

            else
                best
        )
        r
        (List.filter
            (\s ->
                (s.kind == Para)
                    && (s.id /= draftId)
                    && (s.from <= line)
                    && (line < s.to)
                    && List.member r.id (ownersOf m s.id)
            )
            m.rows
        )


{-| ROWS with an EMPTY paragraph drawn in under the stop ID. Zero-width and
holding its MARKER alone, so its text has not moved off its `was`.
-}
drafted : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe (List Row)
drafted m id caret =
    Maybe.map (\j -> joined m j.under (draftRow j j.marker)) (joinAt m id caret)


{-| ROWS with that paragraph filled with TEXT, which is the write. It takes the
DRAFT'S OWN CARET, `draftRow` measuring the marker to indent continuations by.
-}
insertion :
    { a | rows : List Row, lines : List String }
    -> String
    -> Maybe Int
    -> String
    -> Maybe (List Row)
insertion m id caret text =
    Maybe.map (\j -> joined m j.under (draftRow j text)) (joinAt m id caret)


{-| The FIRST LINE the paragraph joined under ID would take, for a cursor that
must land before the rescan mints a row: block ids are POSITIONAL.
-}
joinLine : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe Int
joinLine m id caret =
    Maybe.map
        (\j ->
            -- A REGION'S OWN LINE owes no blank above, where a paragraph's does.
            if j.alone && j.line > 1 && not (blankAt (j.line - 1) m.lines) then
                j.line + 1

            else
                j.line
        )
        (joinAt m id caret)


outermost : { a | rows : List Row } -> Row -> Row
outermost m r =
    case List.reverse (ownersOf m r.id) of
        top :: _ ->
            Maybe.withDefault r (rowById m top)

        [] ->
            r


{-| WRITTEN wearing the blank lines that keep it a paragraph of its own at LINE.
Line 0 is the entry's own headline, the one place nothing is owed above.
-}
apart : List String -> Int -> List String -> List String
apart lines line written =
    (if line > 1 && not (blankAt (line - 1) lines) then
        [ "" ]

     else
        []
    )
        ++ written
        ++ (if blankAt line lines then
                []

            else
                [ "" ]
           )


{-| The row a draft stands in, wearing TEXT exactly: the marker is already in the
box, so this prepends NOTHING and `was` stays the MARKER.
-}
draftRow : Join -> String -> Row
draftRow j text =
    { blank
        | id = draftId
        , kind = Para
        , grain =
            case j.owner of
                Just _ ->
                    Leaf

                Nothing ->
                    Element
        , owner = j.owner
        , from = j.line
        , to = j.line
        , text = riding (String.length j.marker) text
        , was = j.marker
        , alone = j.alone
    }


{-| TEXT with every line but the first indented by N, so a multi-line item stays
ONE item: a continuation at column 1 closes the run in org.
-}
riding : Int -> String -> String
riding n text =
    if n == 0 then
        text

    else
        String.join ("\n" ++ String.repeat n " ") (String.split "\n" text)


{-| ROWS with ROW put in after UNDER and everything UNDER owns UP TO ROW'S OWN
LINE, any standing draft taken out first — the row order its bytes will be in.
-}
joined : { a | rows : List Row } -> String -> Row -> List Row
joined m under row =
    let
        kept =
            List.filter (\r -> r.id /= draftId) m.rows

        owned r =
            r.from < row.from && List.member under (ownersOf { rows = kept } r.id)

        place out rows =
            case rows of
                [] ->
                    out

                r :: rest ->
                    if r.id == under then
                        let
                            kin =
                                takeWhileList owned rest
                        in
                        out ++ (r :: kin) ++ (row :: List.drop (List.length kin) rest)

                    else
                        place (out ++ [ r ]) rest
    in
    place [] kept


placeOfLine : { a | rows : List Row, at : Int } -> Int -> Int
placeOfLine m line =
    List.indexedMap Tuple.pair m.rows
        |> List.filter (\( _, r ) -> r.kind == Para && r.from == line)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault m.at


undrafted : { a | rows : List Row } -> List Row
undrafted m =
    List.filter (\r -> r.id /= draftId) m.rows



-- CURSOR AND GRAIN


rowById : { a | rows : List Row } -> String -> Maybe Row
rowById m id =
    List.head (List.filter (\r -> r.id == id) m.rows)


rowAt : { a | rows : List Row, at : Int } -> Maybe Row
rowAt m =
    nth m.at m.rows


placeOf : { a | rows : List Row, at : Int } -> String -> Int
placeOf m id =
    List.indexedMap (\i r -> ( i, r.id )) m.rows
        |> List.filter (\( _, rid ) -> rid == id)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault m.at


shown : Row -> List Cell
shown r =
    List.filter (\c -> c.val /= "") r.cells


kidsOf : { a | rows : List Row } -> String -> Int
kidsOf m id =
    List.length (List.filter (\r -> r.kind == Para && r.owner == Just id) m.rows)


kindWord : Kind -> String
kindWord k =
    case k of
        Head ->
            "head"

        Para ->
            "para"

        Child ->
            "child"
