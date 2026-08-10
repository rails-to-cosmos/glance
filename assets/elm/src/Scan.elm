module Scan exposing
    ( Cell
    , Grain(..)
    , Kind(..)
    , Row
    , blank
    , blocksIn
    , bodyText
    , cellCount
    , cut
    , insertion
    , kidsOf
    , kindWord
    , listOpener
    , listRun
    , nth
    , placeOf
    , rowAt
    , rowById
    , rowsFrom
    , shown
    )

{-| THE DOCUMENT PANE'S PURE HALF: the structure a subtree's body has, the rows
it becomes, the splice that composes one back, and the readings a cursor is
moved by.

Split out of `Doc` so it can be tested as what it is — functions over lines —
rather than only through a page that has to be booted to ask.

-}


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
    }


blank : Row
blank =
    Row "" Para Element Nothing Nothing 0 0 "" "" [] 0 1



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


{-| The Nth of a list, which ten sites were spelling out.
-}
nth : Int -> List a -> Maybe a
nth i xs =
    List.head (List.drop i xs)


at : Int -> List String -> String
at i xs =
    Maybe.withDefault "" (nth i xs)


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

            else if
                isBlank (at j lines)
                    || listOpener (at j lines)
                    /= Nothing
                    || blockName (at j lines)
                    /= Nothing
                    || isTable (at j lines)
            then
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


rowsFrom : List String -> Int -> List Cell -> List ( Int, Int, List Cell ) -> List Row
rowsFrom lines own headCells kids =
    let
        head =
            { blank | id = "H", kind = Head, grain = Element, cells = headCells }

        blocks =
            blocksIn lines own

        ids =
            List.indexedMap (\i _ -> "B" ++ String.fromInt i) blocks

        idAt k =
            Maybe.withDefault "" (nth k ids)

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



-- THE INSERT
--
-- A paragraph JOINS by growing the structure it lands under, so the splice
-- above is the whole mechanism and no row is made that the reader has not
-- written. One exception: a body holding no block has nothing to grow.


{-| ROWS with a paragraph spelling WRITTEN joined under the stop ID.

A LEAF's rides its OUTERMOST owner — grown in place, org would close the list,
cut the table or take the prose for source. The HEADLINE's leads the body,
joined to the FIRST block from the front. A body with no block at all is SEEDED
with one, the only row this ever makes. 'Nothing' for a CHILD, whose bytes are
outside this window, and for an id no row wears.

-}
insertion :
    { a | rows : List Row, lines : List String }
    -> String
    -> String
    -> Maybe (List Row)
insertion m id written =
    case rowById m id of
        Nothing ->
            Nothing

        Just r ->
            case r.kind of
                Child ->
                    Nothing

                Head ->
                    case List.filter (\x -> x.kind == Para) m.rows of
                        first :: _ ->
                            Just (grown m first.id (written ++ "\n\n" ++ first.text))

                        [] ->
                            Just (seeded m written)

                Para ->
                    let
                        up =
                            outermost m r
                    in
                    Just (grown m up.id (up.text ++ "\n\n" ++ apart m.lines up.to written))


{-| The structure a stop belongs to, itself where it belongs to none.
-}
outermost : { a | rows : List Row } -> Row -> Row
outermost m r =
    case List.reverse (ownersOf m r.id) of
        top :: _ ->
            Maybe.withDefault r (rowById m top)

        [] ->
            r


{-| WRITTEN with the blank line that keeps it a paragraph of its own. Prose at
LINE runs on, and reads back as ONE paragraph with this; the end of the file is
a blank by 'at''s own answer.
-}
apart : List String -> Int -> String -> String
apart lines line written =
    if isBlank (at line lines) then
        written

    else
        written ++ "\n"


{-| Grow ID's range to TEXT. ONE GRAIN SPEAKS FOR A RANGE, so a composite grown
this way silences its own leaves and the structure splices once.
-}
grown : { a | rows : List Row } -> String -> String -> List Row
grown m id text =
    List.map
        (\r ->
            if r.id == id then
                { r | text = text }

            else
                r
        )
        m.rows


{-| The body's first paragraph where there was none: a ZERO-WIDTH range at line
1, under the headline's own line, which the splice takes as an insert with no
arm of its own. It wears \`B0' because that is the id the rescan will mint.
-}
seeded : { a | rows : List Row, lines : List String } -> String -> List Row
seeded m written =
    let
        row =
            { blank
                | id = "B0"
                , kind = Para
                , grain = Element
                , from = 1
                , to = 1
                , text = apart m.lines 1 written
            }
    in
    List.concatMap
        (\r ->
            if r.kind == Head then
                [ r, row ]

            else
                [ r ]
        )
        m.rows



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
