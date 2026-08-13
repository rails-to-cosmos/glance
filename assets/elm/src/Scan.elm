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
moved by.

Split out of `Doc` so it can be tested as what it is — functions over lines —
rather than only through a page that has to be booted to ask.

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

    -- WHAT THE REGION SAID about its continuation: a paragraph stands between
    -- blank lines and every other region's lines sit against their neighbours.
    -- The draft carries it because the SPLICE is what writes the separator.
    , alone : Bool
    }


blank : Row
blank =
    Row "" Para Element Nothing Nothing 0 0 "" "" [] 0 1 False



-- THE LINE PREDICATES
--
-- What counts as an opener, a block, a drawer, a table and a run, settled once
-- and read by everything below. A port of the shell's own, rule for rule: the
-- openers are the corpus's — `-`, `1.`/`1)`, `+`, and an INDENTED `*`, a `* ` at
-- column 1 being a headline.


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
                -- `numberedAt' answers the DIGITS; the punctuation is the
                -- character behind them, and both are the token.
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


{-| The horizontal run a token is followed by, ONE SPACE where the line ends at
the token — `-' alone opens an item, and a sibling of it owes a space.
-}
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


{-| The NUMBER a numbered item opens with, off its whole line.
-}
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


{-| A DRAWER'S NAME: `:NAME:` alone on its line, org's own charset. `:END:` is
the closer rather than an opener, and a colon inside the name declines the line
— `:a:b:` is text, as org reads it.
-}
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


{-| Where the drawer opened at I closes, or -1 — `blockRun`'s rule with org's
one closer in place of a named one.
-}
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


{-| Where a table's run of pipe rows ends. A BLANK LINE ENDS A TABLE, so a pipe
row under one opens a table of its own.
-}
tableEnd : Array String -> Int -> Int -> Int
tableEnd lines end j =
    if j < end && isTable (at j lines) then
        tableEnd lines end (j + 1)

    else
        j


{-| The horizontal run a line opens with, which is what a continuation wears.
-}
indentOf : String -> String
indentOf line =
    String.left (String.length line - String.length (String.trimLeft line)) line


isBlank : String -> Bool
isBlank line =
    String.trim line == ""


{-| A line that RIDES INSIDE the item above it: an opener of its own, or an
indented continuation.
-}
rides : String -> Bool
rides line =
    listOpener line /= Nothing || String.startsWith " " line || String.startsWith "\t" line


{-| The Nth of a list, which ten sites were spelling out.
-}
nth : Int -> List a -> Maybe a
nth i xs =
    List.head (List.drop i xs)


{-| THE NTH LINE OF THE BODY, and it reads an `Array` because the walks below ask
for one line after another: `List.drop i` is O(i), which made a 2,178-line body
cost nine million cons cells and 75 ms where an array costs 11.
-}
at : Int -> Array String -> String
at i xs =
    Maybe.withDefault "" (Array.get i xs)


{-| Whether the Nth line of the SPLICE'S OWN list is blank. The splice COMPOSES
lines where the walks step through them, so it keeps the shape the model holds.
-}
blankAt : Int -> List String -> Bool
blankAt i xs =
    isBlank (Maybe.withDefault "" (nth i xs))


cut : List String -> Int -> Int -> String
cut lines a b =
    String.join "\n" (List.take (b - a) (List.drop a lines))


{-| Where the block opened at I closes, or -1.
-}
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
--
-- ONE RECOGNIZER, and every walk below reads it: which kind a line opens, where
-- that kind ends, and whether it carries a closer. Three total cases over one
-- sum, so a sixth kind fails the build in each. Two of them spelled their set
-- as a comparison once, and a comparison answers silently.


type RegionKind
    = Plain
    | Item
    | Table
    | Block
    | Drawer


{-| THE KIND THE LINE AT I OPENS, `Plain` where it opens nothing. The five kinds
read the SAME predicates everything else does (`drawerName`, `blockName`,
`isTable`, `listOpener`), so what counts as a drawer, a block, a table or a list
is settled here and nothing below widens it.
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
walk: a DRAWER opens no stop, so its opener and its closer are prose lines and
what it holds is whatever else those lines spell. Stated here once, where the
walk and the scanner meet.
-}
stopKindAt : Array String -> Int -> RegionKind
stopKindAt lines i =
    case kindAt lines i of
        Drawer ->
            Plain

        kind ->
            kind


{-| WHICH KINDS CARRY A CLOSER, asked wherever a boundary line is the question:
what a region's INTERIOR is (`interiorEnd`), whether a line is its CLOSING one
(`closerAt`), and whether an opener is one a run must step over whole
(`closedRun`).
-}
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


{-| ONE PAST THE LAST LINE of the KIND opened at I, and `-1` where a kind that
CLOSES never does. A caller wanting an ITEM's own boundaries reads `listRun` for
them; this answers where its whole RUN ends.
-}
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


{-| Where a prose run ends: the next blank line, or the next line that opens
something — READ THROUGH THE CALLER'S OWN CLASSIFIER, which is what makes the
scanner's paragraph run past a drawer where the walk's stops at one.
-}
proseEnd : (Array String -> Int -> RegionKind) -> Array String -> Int -> Int -> Int
proseEnd kind lines end j =
    if j >= end || isBlank (at j lines) || kind lines j /= Plain then
        j

    else
        proseEnd kind lines end (j + 1)


{-| Where the region opened at J closes, or `Nothing` — asked of the kinds that
CLOSE, so a caller wanting "is this line the opener of something that ends?" asks
once.
-}
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


{-| ONE BLANK LINE STAYS IN — org's rule. Two close the list, as does a blank
with something that does not ride under it.

AND A BLOCK OR A DRAWER RIDING INSIDE THE RUN IS STEPPED OVER WHOLE, so no item
boundary is cut through one — org's `org-list-struct` again.

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
                            -- A BLOCK OR A DRAWER IS ONE SYNTACTIC UNIT, so an
                            -- item boundary is never cut through one: the run
                            -- steps over it whole.  org's own `org-list-struct'
                            -- does exactly this ("skip block or drawer at point,
                            -- and move to next line"), and without it a `- b'
                            -- between two source lines ended the item above,
                            -- leaving the `#+begin_src' with no closer in it.
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
--
-- ONE QUESTION — WHICH REGION HOLDS THIS LINE — and both consumers read the one
-- answer: the STRUCTURE SCANNER asks it before minting a stop INSIDE a stop, and
-- the MARKER rule asks it what a new line there opens with. The two walked
-- separately once and disagreed, which is what every bug in this area was: a
-- bullet inside a nested block was an ITEM to the scanner and the BLOCK's line to
-- the marker rule, so the pane grew a stop whose deletion left the block open.
--
-- HOW DEEP IT GOES IS ORG'S GREATER/LESSER SPLIT (`greater'): the walk RE-ENTERS
-- a greater region and treats a lesser one as OPAQUE. Re-entering the item alone
-- left a table inside a `#+begin_pin' answered with the block's empty line, which
-- split the table in two.


{-| A region's kind and the lines it covers — `to` one past the last, so the
CLOSER of a kind that has one is `to - 1`. The MARKER is derived rather than
stored (`markerFor`), a table's being measured off the rows it already spells.
-}
type alias Region =
    { kind : RegionKind, from : Int, to : Int }


{-| THE REGIONS between FROM and END, in line order, each holding its own lines.
The kind of every one is `kindAt`'s, so what counts as a list, a table, a block
or a drawer is settled in one place and this widens none of it.

ITEMS TILE THE RUN they sit in, since org keeps one blank line inside a list and
a line no item opened is the item above it. A STOP cut from an item is `snug`,
which gives those blanks back.

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

        -- A kind that CLOSES is the run it closes, and an unclosed opener is the
        -- text org reads it as.
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


{-| ORG'S OWN GREATER/LESSER SPLIT (`org-element-greater-elements`), and it is
what decides re-entry. A GREATER element CONTAINS other elements, so the walk
asks the same question inside it: an ITEM, a DRAWER, and every block org parses
the contents of — `center`, `quote`, and any SPECIAL block a tree spells a name
of its own. A LESSER element holds no elements and is OPAQUE: the VERBATIM
blocks, where org SUSPENDS its own grammar — which is why an empty line is the
right continuation in one and why a bullet inside one is source rather than an
item.

A TABLE IS GREATER IN ORG and a leaf here, the one name where this walk departs
from that list. Org's `table` contains `table-row` and nothing else, so
re-entering it would answer a caret with a row — which is what the Table marker
already spells.

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


{-| THE VERBATIM BLOCKS, and the list is the five names
`org-element-greater-elements` leaves out: org parses no element inside one, so
nothing there is a table, a drawer or an item either. Every other `#+begin_X` is
a SPECIAL block and greater, so a tree's own `#+begin_pin` holding a table holds
a TABLE.

ORG'S LIST RULE IS A DIFFERENT VARIABLE AND A SHORTER LIST.
`org-list-forbidden-blocks` — "names of blocks where lists are not allowed" —
names four, sparing `comment`, and it answers about LISTS alone. The question
here is the ELEMENT one, so the element list is what it takes.

-}
verbatim : String -> Bool
verbatim name =
    List.member name [ "comment", "example", "export", "src", "verse" ]


{-| The region holding LINE, sought between FROM and END — the stop's own
structure, so a region never reaches into a child's lines.

A LINE NO REGION CLAIMS IS PROSE OF ITS OWN, and two kinds of line reach that
answer: a BLANK line between two regions, which no region tiles, and the OPENER
or the CLOSER of a greater region the walk has just re-entered, neither being an
interior line. In the second case `within` reads the `Plain` and hands the region
itself back.

-}
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


{-| THE WALK RE-ENTERS A GREATER REGION. Its contents are elements — an item's
lines are a body of their own, and so are a drawer's and a quote block's — so the
same question is asked again inside it and a nested region's answer stands.

WHAT NO NESTED REGION CLAIMS IS THE GREATER ONE'S, and so is the CLOSING line of
a nested region: it asks for what comes AFTER that region, and inside a greater
one that is the greater one. The span shrinks at every step, so the recursion
ends where the nesting does.

-}
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


{-| ONE PAST A REGION'S LAST INTERIOR LINE. A kind that CLOSES keeps its closer
out of them and every other kind's contents run to its end — so a caret on either
boundary line finds no interior region and falls back to the region itself.
-}
interiorEnd : Region -> Int
interiorEnd reg =
    if closes reg.kind then
        reg.to - 1

    else
        reg.to


{-| A region's CLOSING line, where the kind has one. A TABLE has none, so a
caret on its last row keeps the new row inside it.
-}
closerAt : Region -> Int -> Bool
closerAt reg line =
    closes reg.kind && line == reg.to - 1


{-| A STOP ENDS AT ITS LAST LINE WITH SOMETHING ON IT, where a region tiles the
run it sits in. One rule, asked wherever a stop is cut from a region.
-}
snug : Array String -> Int -> Int -> Int
snug lines from to =
    if to > from && isBlank (at (to - 1) lines) then
        snug lines from (to - 1)

    else
        to



-- THE STRUCTURE SCANNER
--
-- What the pane STOPS on: the composites, their leaves, and the ladder between
-- them. It reads `kindAt' at the top level and `regionsIn' inside a stop, so the
-- recognition is the walk's throughout and only the SHAPE of what each kind
-- becomes is this section's.


{-| The non-blank runs between A and B, each as its own leaf.
-}
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


{-| The body's structure, emitted INLINE as `[whole, leaf1..leafN]` — the walk
reads that order. `up` indexes OUT, at the leaf's IMMEDIATE owner, so the grain
is a ladder. OWN is the server's `ownLines`, which is what keeps a child's bytes
out of the body's paragraphs.

A DRAWER IS NO STOP — it is a REGION, drawn as the paragraph it sits in — and
NEITHER IS A BLOCK OR A TABLE RIDING INSIDE AN ITEM: such an item is ONE stop
holding every one of its lines. Stops there would move the GRAIN — what `n`/`p`
steps and what `d` takes — which is a different question from where a new line
goes.

INSIDE A STOP THE REGION WALK SAYS WHAT MAY BECOME ONE (`regionsIn`), which
names the regions at ONE level and stops: a nested block or drawer is that one
region, so nothing it holds mints a stop of its own. At the TOP level the kinds
are `kindAt`'s, and the ONE divergence is the `Drawer` arm below — its opener and
its closer are the paragraph's lines, which is `stopKindAt`.

-}
blocksIn : Array String -> Int -> List Stop
blocksIn lines own =
    let
        end =
            max 0 (min own (Array.length lines))

        -- A list item, plus any ITEM the walk names inside it.
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
                    -- A DRAWER IS NO STOP, so its opener falls to the paragraph
                    -- and `stopKindAt' carries the rest of its lines there.
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
--
-- ONE GRAIN SPEAKS FOR A RANGE: a composite and its leaves cover the same
-- lines, so a moved or going ancestor silences every rung under it. Bottom-up,
-- so an earlier range is never moved by a later splice.


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
                            -- A PARAGRAPH STANDS APART, and the blanks that keep
                            -- it one are the SPLICE's rather than the text's: a
                            -- zero-width range ADDS lines instead of replacing
                            -- any, and what is drawn is content.  Every other
                            -- region's continuation sits against its neighbours
                            -- — a blank above an item would put the run's own
                            -- separator in front of a sibling, and a blank
                            -- inside a table would END it.
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
--
-- ONE ANSWER PER REGION, off the one walk above: what a new line inside it
-- opens with.


{-| ONE ANSWER PER REGION: what a new line inside it opens with.

  - bullet item — its own bullet, org's `M-RET`
  - checkbox item — bullet plus an EMPTY box, a new task being undone
  - numbered item — the next number, the duplicate below being
    `org-list-repair`'s business
  - table — an empty ROW aligned to the table's own widths, a blank line
    ending a table
  - `#+begin_X` block — empty, at the block's indent, source being no org markup
  - drawer, `:LOGBOOK:` included — empty, at the drawer's indent
  - paragraph — empty, and the blank lines that set it apart are the splice's

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


{-| INDENT AND BULLET ARE THE ITEM'S OWN, which is org's `org-insert-item` and
what keeps a `[2/4]` cookie counting the same kind of thing.
-}
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


{-| That line's own bullet, except where the run is NUMBERED: there the number
continues off the line consulted, and only the punctuation is the line's.
-}
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


{-| An EMPTY box where what follows the bullet is one, whatever state it is in.
-}
boxAfter : String -> String
boxAfter after =
    if List.member (String.left 3 after) [ "[ ]", "[X]", "[x]", "[-]" ] then
        "[ ] "

    else
        ""


{-| ORG'S OWN ALIGNMENT: a pipe, then each column padded to the width the table
already spells, pipe-separated. RULE ROWS are out of the measurement — their
dashes are as wide as the column and say nothing about it.
-}
tableRow : Array String -> Int -> Int -> String
tableRow lines from to =
    Array.toList (Array.slice from to lines)
        |> List.filter (not << isRule)
        |> List.map (List.map String.length << tableCells)
        |> List.foldl widest []
        |> List.map (\w -> String.repeat w " " ++ "|")
        |> String.concat
        |> (++) "|"


{-| org's `|---+---|`, which its dashes make as wide as the column.
-}
isRule : String -> Bool
isRule line =
    String.startsWith "|-" (String.trimLeft line)


{-| What sits BETWEEN a row's pipes, the empty tail a closing pipe leaves
dropped.
-}
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


{-| WHERE POINT GOES IN A MARKER: at its end, a marker being a LEAD the reader
types after — except a TABLE ROW, which is a whole row and closes with a pipe.
Typing past that pipe opens a column org's own align would then keep, so point
goes inside the FIRST CELL, one space in where the cell has the room.
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



-- THE INSERT
--
-- `+' ADDS A SIBLING OF THE STOP, and a sibling is a MARKER plus the reader's
-- own text. It is DRAWN before it is written: a ZERO-WIDTH row goes in wearing
-- the marker alone, and `bodyText' passes it over because its text has not
-- moved off its `was'. So the reader sees the line they are about to fill as
-- the item it will be, and not a byte is owed until they fill it.


{-| The row a paragraph waits in before it says anything.
-}
draftId : String
draftId =
    "D"


{-| WHERE a sibling joins: the row it goes in under, the BODY LINE it takes, the
MARKER it opens wearing before a character is typed, whether it is a paragraph
the splice owes blank lines to, and THE WORD FOR IT, which the shell echoes.

The word rides here because every branch below already knows it: a second
reading on the page called a table row inside a list item "an item at this
level".

-}
type alias Join =
    { under : String
    , line : Int
    , marker : String
    , owner : Maybe String
    , alone : Bool
    , word : String
    }


{-| THE WORD FOR WHERE `+' WOULD LAND, and `Nothing` where nothing takes a
paragraph — a CHILD, or an id no row wears.
-}
joinWord : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe String
joinWord m id caret =
    Maybe.map .word (joinAt m id caret)


{-| WHAT A PARAGRAPH RIDING PAST A STOP IS CALLED: the structure it clears where
the stop belongs to one, and the stop itself where it does not.
-}
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


{-| AND WHAT A LINE JOINING A REGION IS CALLED, one per kind. `Plain` is the
region a paragraph rides past, which `inside` answers before it asks here.
-}
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


{-| WHERE `+' joins. The HEADLINE's leads the body at line 1, under the line the
entry wears, which is the one place nothing is owed above. `Nothing` for a CHILD,
whose bytes are outside this window, and for an id no row wears.

A CARET is a line INSIDE the stop, where `S-RET' was pressed in a box holding
several lines, and it is what makes a region's interior addressable. Without one
there is nothing to be inside, so the answer is a sibling of the STOP.

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


{-| `+' WITH NO BOX OPEN NAMES NO LINE, so THE GRAIN IS THE SELECTOR: a LEAF of a
list takes a sibling of the stop, wearing the stop's own opener and joining past
its whole nested run, and everything else rides past the structure it belongs to
— the paragraph after a list is one `b' from any item and is reachable no other
way.
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


{-| THE REGION HOLDING THE CARET'S LINE ANSWERS, and it answers both halves: its
marker is what the new line opens with, and its interior is where that line goes
— immediately under the caret's own, so the run splits where the reader stands
and everything below it stays below.

A CLOSING LINE (`#+end_X`, `:END:`) is the region's last and a caret on it asks
for what comes AFTER: the line lands past the region, wearing the continuation of
whatever holds it there — prose at the top level, the ITEM where the region rode
inside one. A TABLE has no closer, so a caret on its last row keeps the new row
inside it, which is how a table is actually built.

A PARAGRAPH takes the same answer it always did — a paragraph of its own past the
prose — since its lines are one value rather than a sequence of slots.

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


{-| THE OWNER IS THE ANCHORED LINE'S — the deepest stop holding the line above
it, or that stop's owner where the sibling clears it. `Doc.viewKids' walks a
composite's kids only while their owner is its own, so an owner disagreeing with
the line breaks the walk and the leaves past it are drawn a SECOND time as the
gap text. Every byte on screen exactly once is the rule that says so.
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


{-| THE LINE A CARET NAMES, absolute and clamped to the stop's own last.
-}
caretLine : Row -> Int -> Int
caretLine r off =
    r.from + clamp 0 (max 0 (r.to - r.from - 1)) off


{-| The DEEPEST stop holding LINE: the rung nested furthest inside R that still
covers it, R itself where none does.
-}
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


{-| ROWS with an EMPTY paragraph drawn in under the stop ID, for \`+' to open a
box over. It is zero-width and holds its MARKER and nothing else, so its text has
not moved off its `was' and no write any other gesture composes can carry it out.
-}
drafted : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe (List Row)
drafted m id caret =
    Maybe.map (\j -> joined m j.under (draftRow j j.marker)) (joinAt m id caret)


{-| ROWS with that paragraph filled with TEXT, which is the write.

THE SEPARATOR IS DECIDED rather than spelled: a blank ABOVE unless the headline
is the line above, and one BELOW only where the line the row takes is prose that
would otherwise read back as ONE paragraph with this.

It takes the DRAFT'S OWN CARET, since `draftRow' measures the marker to indent a
multi-line item's continuations by: reading a different line here would ride the
reader's second line under a bullet it never wore, and land the write somewhere
the draw never drew.

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
must land on a row the rescan has not minted: block ids are POSITIONAL, so no
id names it until the body comes back.

It asks under the CARET the write went out under, the two answering one question.

-}
joinLine : { a | rows : List Row, lines : List String } -> String -> Maybe Int -> Maybe Int
joinLine m id caret =
    Maybe.map
        (\j ->
            -- A REGION'S OWN LINE owes no blank above, so its landing is the
            -- line it takes exactly, where a paragraph's is one past the blank
            -- written over it.
            if j.alone && j.line > 1 && not (blankAt (j.line - 1) m.lines) then
                j.line + 1

            else
                j.line
        )
        (joinAt m id caret)


{-| The structure a stop belongs to, itself where it belongs to none.
-}
outermost : { a | rows : List Row } -> Row -> Row
outermost m r =
    case List.reverse (ownersOf m r.id) of
        top :: _ ->
            Maybe.withDefault r (rowById m top)

        [] ->
            r


{-| WRITTEN wearing the blank lines that keep it a paragraph of its own at LINE.

One ABOVE unless line 0 is what sits there — the entry's own headline line, and
the one place nothing is owed — and one BELOW where the line it lands on is
prose that would otherwise read back as ONE paragraph with this.

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


{-| The row a draft stands in, wearing TEXT exactly.

WHAT THE BOX HOLDS IS WHAT IS WRITTEN. The marker is drawn into the box the
moment `+' is pressed, so it arrives back as part of the line and this prepends
NOTHING: a reader who edits `- [ ] ' into `- DONE' gets `- DONE', where a prepend
would have made it `- [ ] - DONE'. `was' stays the MARKER, which is what keeps an
untouched draft out of `bodyText'.

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
ONE item: a continuation at column 1 closes the run in org and reads back as a
paragraph, where a line opening in space RIDES INSIDE the item above it. An
empty MARKER indents nothing, a paragraph owing its neighbours blank lines
rather than an indent.
-}
riding : Int -> String -> String
riding n text =
    if n == 0 then
        text

    else
        String.join ("\n" ++ String.repeat n " ") (String.split "\n" text)


{-| ROWS with ROW put in after UNDER and everything UNDER owns UP TO ROW'S OWN
LINE, with any draft already standing taken out first, so a second ask draws one
paragraph rather than two.

THE LINE IS WHERE THE WALK STOPS, which is what puts a sibling splitting a nested
run in the row order its bytes will be in. Past the whole structure the walk
takes every rung, since every one of them opens above the line.

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


{-| Where the row taking LINE as its first stands, for the landing above.
-}
placeOfLine : { a | rows : List Row, at : Int } -> Int -> Int
placeOfLine m line =
    List.indexedMap Tuple.pair m.rows
        |> List.filter (\( _, r ) -> r.kind == Para && r.from == line)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault m.at


{-| ROWS with no draft standing: what \`ESC' leaves behind, which is what it
found.
-}
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
