module Scan exposing
    ( Cell
    , Grain(..)
    , Kind(..)
    , RegionKind(..)
    , Row
    , blank
    , blocksIn
    , bodyText
    , caretIn
    , cellCount
    , cut
    , draftId
    , drafted
    , insertion
    , joinLine
    , joinWord
    , kidsOf
    , kindWord
    , listOpener
    , markerFor
    , nth
    , placeOf
    , placeOfLine
    , regionAt
    , rowAt
    , rowById
    , rowsFrom
    , shown
    , undrafted
    )

{-| THE DOCUMENT PANE'S PURE HALF: the structure a subtree's body has, the rows
it becomes, the splice that composes one back, and the readings a cursor is
moved by. Split out of `Doc` so it can be tested as functions over lines.
-}

import Array exposing (Array)


type Grain
    = Element
    | Composite
    | Leaf


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



-- THE LINE PREDICATES


{-| An item's opener: how far in it sits, and what it OPENS WITH — the token
plus the horizontal run behind it, which is the prefix `+' spells a sibling with.
-}
type alias Opener =
    { indent : Int, bullet : String }


listOpener : String -> Maybe Opener
listOpener line =
    let
        spaces =
            String.length line - String.length (String.trimLeft line)

        rest =
            String.dropLeft spaces line

        tokenAt =
            if String.startsWith "- " rest || rest == "-" then
                Just "-"

            else if String.startsWith "+ " rest || rest == "+" then
                Just "+"

            else if String.startsWith "* " rest || rest == "*" then
                Just "*"

            else
                Maybe.map
                    (\d -> d ++ String.slice (String.length d) (String.length d + 1) rest)
                    (numberedAt rest)

        opened token =
            Opener spaces
                (token ++ gapAfter (String.dropLeft (String.length token) rest))
    in
    case tokenAt of
        Nothing ->
            Nothing

        -- A `* ' at COLUMN 1 is a headline rather than an item.
        Just "*" ->
            if spaces == 0 then
                Nothing

            else
                Just (opened "*")

        Just token ->
            Just (opened token)


gapAfter : String -> String
gapAfter after =
    let
        run =
            String.toList after
                |> takeWhileList (\c -> c == ' ' || c == '\t')
                |> String.fromList
    in
    if String.isEmpty run then
        " "

    else
        run


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

    else if
        String.startsWith ". " after
            || String.startsWith ") " after
            || after
            == "."
            || after
            == ")"
    then
        Just digits

    else
        Nothing


numberAt : String -> Maybe Int
numberAt line =
    let
        spaces =
            String.length line - String.length (String.trimLeft line)
    in
    Maybe.andThen String.toInt (numberedAt (String.dropLeft spaces line))


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


endsBlock : String -> String -> Bool
endsBlock name line =
    String.toLower (String.trim line) == "#+end_" ++ name


drawerName : String -> Maybe String
drawerName line =
    let
        body =
            String.trim line

        inner =
            String.slice 1 (String.length body - 1) body
    in
    if
        String.startsWith ":" body
            && String.endsWith ":" body
            && String.length body > 2
            && String.all drawerChar inner
            && String.toUpper inner
            /= "END"
    then
        Just (String.toUpper inner)

    else
        Nothing


drawerChar : Char -> Bool
drawerChar c =
    Char.isAlphaNum c || c == '-' || c == '_'


drawerEnds : String -> Bool
drawerEnds line =
    String.toUpper (String.trim line) == ":END:"


drawerRun : Array String -> Int -> Int -> Int
drawerRun lines i end =
    let
        go j =
            if j >= end then
                -1

            else if drawerEnds (at j lines) then
                j + 1

            else
                go (j + 1)
    in
    go (i + 1)


isTable : String -> Bool
isTable line =
    String.startsWith "|" (String.trimLeft line)


tableEnd : Array String -> Int -> Int -> Int
tableEnd lines end j =
    if j < end && isTable (at j lines) then
        tableEnd lines end (j + 1)

    else
        j


indentOf : String -> String
indentOf line =
    String.left (String.length line - String.length (String.trimLeft line)) line


isBlank : String -> Bool
isBlank line =
    String.trim line == ""


rides : String -> Bool
rides line =
    listOpener line /= Nothing || String.startsWith " " line || String.startsWith "\t" line


nth : Int -> List a -> Maybe a
nth i xs =
    List.head (List.drop i xs)


{-| THE NTH LINE OF THE BODY, off an `Array`: the walks ask for one line after
another and `List.drop i` is O(i). The SPLICE keeps a `List` (`blankAt`).
-}
at : Int -> Array String -> String
at i xs =
    Maybe.withDefault "" (Array.get i xs)


blankAt : Int -> List String -> Bool
blankAt i xs =
    isBlank (Maybe.withDefault "" (nth i xs))


cut : List String -> Int -> Int -> String
cut lines a b =
    String.join "\n" (List.take (b - a) (List.drop a lines))


blockRun : Array String -> Int -> Int -> String -> Int
blockRun lines i end name =
    let
        go j =
            if j >= end then
                -1

            else if endsBlock name (at j lines) then
                j + 1

            else
                go (j + 1)
    in
    go (i + 1)


-- WHAT A LINE OPENS


type RegionKind
    = Plain
    | Item
    | Table
    | Block
    | Drawer


{-| THE KIND THE LINE AT I OPENS, `Plain` where it opens nothing. The five read
the SAME predicates everything else does, and nothing below widens them.
-}
kindAt : Array String -> Int -> RegionKind
kindAt lines i =
    let
        line =
            at i lines
    in
    if drawerName line /= Nothing then
        Drawer

    else if blockName line /= Nothing then
        Block

    else if isTable line then
        Table

    else if listOpener line /= Nothing then
        Item

    else
        Plain


{-| THE STRUCTURE SCANNER'S OWN READING, and its ONE divergence from the region
walk: a DRAWER opens no stop, so its opener and its closer are prose lines.
-}
stopKindAt : Array String -> Int -> RegionKind
stopKindAt lines i =
    case kindAt lines i of
        Drawer ->
            Plain

        kind ->
            kind


closes : RegionKind -> Bool
closes kind =
    case kind of
        Block ->
            True

        Drawer ->
            True

        Plain ->
            False

        Item ->
            False

        Table ->
            False


extentOf : Array String -> Int -> Int -> RegionKind -> Int
extentOf lines end i kind =
    case kind of
        Plain ->
            proseEnd kindAt lines end (i + 1)

        Item ->
            (listRun lines i end).to

        Table ->
            tableEnd lines end i

        Block ->
            blockRun lines i end (Maybe.withDefault "" (blockName (at i lines)))

        Drawer ->
            drawerRun lines i end


{-| Where a prose run ends, READ THROUGH THE CALLER'S OWN CLASSIFIER — which is
what makes the scanner's paragraph run past a drawer where the walk's stops.
-}
proseEnd : (Array String -> Int -> RegionKind) -> Array String -> Int -> Int -> Int
proseEnd kind lines end j =
    if j >= end || isBlank (at j lines) || kind lines j /= Plain then
        j

    else
        proseEnd kind lines end (j + 1)


closedRun : Array String -> Int -> Int -> Maybe Int
closedRun lines end j =
    let
        kind =
            kindAt lines j

        shut =
            if closes kind then
                extentOf lines end j kind

            else
                -1
    in
    if shut == -1 then
        Nothing

    else
        Just shut


type alias Run =
    { to : Int, items : List ( Int, Int ) }


{-| ONE BLANK LINE STAYS IN — org's rule. A BLOCK OR A DRAWER RIDING INSIDE THE
RUN IS STEPPED OVER WHOLE, org's `org-list-struct` again.
-}
listRun : Array String -> Int -> Int -> Run
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
                            -- ONE SYNTACTIC UNIT: org's `org-list-struct' skips
                            -- a block or a drawer at point whole.
                            case closedRun lines end j of
                                Just shut ->
                                    go shut from shut items

                                Nothing ->
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


-- THE REGION WALK


type alias Region =
    { kind : RegionKind, from : Int, to : Int }


{-| THE REGIONS between FROM and END, in line order, each holding its own lines.
ITEMS TILE THE RUN they sit in, org keeping one blank line inside a list; a STOP
cut from an item is `snug`.
-}
regionsIn : Array String -> Int -> Int -> List Region
regionsIn lines from end =
    let
        go i out =
            if i >= end then
                out

            else if isBlank (at i lines) then
                go (i + 1) out

            else
                case kindAt lines i of
                    Plain ->
                        prose i out

                    Item ->
                        let
                            run =
                                listRun lines i end
                        in
                        go (max (i + 1) run.to) (out ++ items run)

                    Table ->
                        held Table i out

                    Block ->
                        held Block i out

                    Drawer ->
                        held Drawer i out

        -- A kind that CLOSES is the run it closes; an unclosed opener is text.
        held kind i out =
            let
                to =
                    extentOf lines end i kind
            in
            if to == -1 then
                prose i out

            else
                go to (out ++ [ Region kind i to ])

        items run =
            List.map2 (\( a, _ ) b -> Region Item a b)
                run.items
                (List.drop 1 (List.map Tuple.first run.items) ++ [ run.to ])

        prose i out =
            let
                j =
                    extentOf lines end i Plain
            in
            go j (out ++ [ Region Plain i j ])
    in
    go from []


{-| ORG'S OWN GREATER/LESSER SPLIT (`org-element-greater-elements`), which
decides re-entry. A TABLE IS GREATER IN ORG and a leaf here, the one departure.
-}
greater : Array String -> Region -> Bool
greater lines reg =
    case reg.kind of
        Item ->
            True

        Drawer ->
            True

        Block ->
            not (verbatim (Maybe.withDefault "" (blockName (at reg.from lines))))

        Table ->
            False

        Plain ->
            False


{-| THE VERBATIM BLOCKS: the five names `org-element-greater-elements` leaves
out. ORG'S LIST RULE IS A DIFFERENT VARIABLE — `org-list-forbidden-blocks` names
four, spares `comment`, and answers about LISTS alone.
-}
verbatim : String -> Bool
verbatim name =
    List.member name [ "comment", "example", "export", "src", "verse" ]


regionAt : Array String -> Int -> Int -> Int -> Region
regionAt lines from end line =
    case List.filter (\r -> r.from <= line && line < r.to) (regionsIn lines from end) of
        reg :: _ ->
            if greater lines reg then
                within lines reg line

            else
                reg

        [] ->
            Region Plain line (line + 1)


within : Array String -> Region -> Int -> Region
within lines reg line =
    let
        nested =
            regionAt lines (reg.from + 1) (interiorEnd reg) line
    in
    if nested.kind == Plain || closerAt nested line then
        reg

    else
        nested


interiorEnd : Region -> Int
interiorEnd reg =
    if closes reg.kind then
        reg.to - 1

    else
        reg.to


closerAt : Region -> Int -> Bool
closerAt reg line =
    closes reg.kind && line == reg.to - 1


snug : Array String -> Int -> Int -> Int
snug lines from to =
    if to > from && isBlank (at (to - 1) lines) then
        snug lines from (to - 1)

    else
        to



-- THE STRUCTURE SCANNER


runsIn : Array String -> Int -> Int -> List ( Int, Int )
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


type alias Stop =
    { from : Int, to : Int, grain : Grain, name : Maybe String, up : Maybe Int }


{-| The body's structure, emitted INLINE as `[whole, leaf1..leafN]`. `up` indexes
OUT, at the leaf's IMMEDIATE owner, so the grain is a ladder. A DRAWER IS NO
STOP, and neither is a block or a table riding INSIDE an item: stops there would
move the GRAIN, a different question from where a new line goes.
-}
blocksIn : Array String -> Int -> List Stop
blocksIn lines own =
    let
        end =
            max 0 (min own (Array.length lines))

        pushItem from to up out =
            let
                here =
                    List.length out

                deeper reg got =
                    if reg.kind == Item then
                        pushItem reg.from (snug lines reg.from reg.to) (Just here) got

                    else
                        got
            in
            List.foldl deeper
                (out ++ [ Stop from to Leaf Nothing up ])
                (regionsIn lines (from + 1) to)

        whole a b name leaves out =
            let
                here =
                    List.length out
            in
            List.foldl (\( p, q ) got -> got ++ [ Stop p q Leaf Nothing (Just here) ])
                (out ++ [ Stop a b Composite (Just name) Nothing ])
                leaves

        go i out =
            if i >= end then
                out

            else if isBlank (at i lines) then
                go (i + 1) out

            else
                case kindAt lines i of
                    -- A DRAWER IS NO STOP: `stopKindAt' takes its lines to prose.
                    Drawer ->
                        plain i out

                    Plain ->
                        plain i out

                    Block ->
                        let
                            shut =
                                extentOf lines end i Block
                        in
                        if shut == -1 then
                            plain i out

                        else
                            go shut
                                (whole i
                                    shut
                                    (Maybe.withDefault "" (blockName (at i lines)))
                                    (runsIn lines (i + 1) (shut - 1))
                                    out
                                )

                    Table ->
                        let
                            j =
                                extentOf lines end i Table
                        in
                        go j (whole i j "table" (List.map (\n -> ( n, n + 1 )) (List.range i (j - 1))) out)

                    Item ->
                        let
                            run =
                                listRun lines i end

                            here =
                                List.length out

                            opened =
                                out ++ [ Stop i run.to Composite (Just "list") Nothing ]
                        in
                        go (max (i + 1) run.to)
                            (List.foldl (\( a, b ) got -> pushItem a b (Just here) got) opened run.items)

        plain i out =
            let
                j =
                    proseEnd stopKindAt lines end (i + 1)
            in
            go j (out ++ [ Stop i j Element Nothing Nothing ])
    in
    go 1 []



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
    in
    (head :: body) ++ List.map child kids



-- THE SPLICE
-- ONE GRAIN SPEAKS FOR A RANGE; bottom-up, so a later splice moves no earlier one.


ownersOf : { a | rows : List Row } -> String -> List String
ownersOf m id =
    case Maybe.andThen .owner (rowById m id) of
        Nothing ->
            []

        Just up ->
            up :: ownersOf m up


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
                List.take r.from out
                    ++ (if r.alone then
                            -- The blanks that keep a paragraph one are the
                            -- SPLICE's: a zero-width range ADDS lines.
                            apart out r.from (String.split "\n" r.text)

                        else
                            String.split "\n" r.text
                       )
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
