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
    , draftId
    , drafted
    , insertion
    , joinLine
    , kidsOf
    , kindWord
    , listOpener
    , listRun
    , nth
    , placeOf
    , placeOfLine
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
                List.take r.from out
                    ++ (if r.from == r.to then
                            -- AN INSERTION, and the blanks that keep it a
                            -- paragraph of its own are the SPLICE's rather than
                            -- the text's: a zero-width range ADDS lines instead
                            -- of replacing any, and what is drawn is content.
                            apart out r.from (String.split "\n" r.text)

                        else
                            String.split "\n" r.text
                       )
                    ++ List.drop r.to out

            else
                out
    in
    String.join "\n" (List.foldl splice m.lines paras)



-- THE INSERT
--
-- A paragraph is DRAWN before it is written: `+' puts a ZERO-WIDTH row in under
-- the structure the stop rides, holding nothing, and `bodyText' passes it over
-- because its text has not moved off its `was'. So the reader sees the line
-- they are about to fill, and not a byte is owed until they fill it.


{-| The row a paragraph waits in before it says anything.
-}
draftId : String
draftId =
    "D"


{-| WHERE a paragraph joins: the row it goes in under, the BODY LINE it takes,
and whether a blank line is owed ABOVE it.

A LEAF's rides its OUTERMOST owner — grown in place, org would close the list,
cut the table or take the prose for source. The HEADLINE's leads the body at
line 1, under the line the entry wears, which is the one place nothing is owed
above. 'Nothing' for a CHILD, whose bytes are outside this window, and for an
id no row wears.

-}
joinAt : { a | rows : List Row } -> String -> Maybe ( String, Int )
joinAt m id =
    case rowById m id of
        Nothing ->
            Nothing

        Just r ->
            case r.kind of
                Child ->
                    Nothing

                Head ->
                    Just ( r.id, 1 )

                Para ->
                    let
                        up =
                            outermost m r
                    in
                    Just ( up.id, up.to )


{-| ROWS with an EMPTY paragraph drawn in under the stop ID, for \`+' to open a
box over. It is zero-width and holds nothing, so no write any other gesture
composes can carry it out.
-}
drafted : { a | rows : List Row } -> String -> Maybe (List Row)
drafted m id =
    Maybe.map (\( under, line ) -> joined m under (draftRow line "")) (joinAt m id)


{-| ROWS with that paragraph filled with TEXT, which is the write.

THE SEPARATOR IS DECIDED rather than spelled: a blank ABOVE unless the headline
is the line above, and one BELOW only where the line the row takes is prose that
would otherwise read back as ONE paragraph with this.

-}
insertion :
    { a | rows : List Row, lines : List String }
    -> String
    -> String
    -> Maybe (List Row)
insertion m id text =
    Maybe.map (\( under, line ) -> joined m under (draftRow line text)) (joinAt m id)


{-| The FIRST LINE the paragraph joined under ID would take, for a cursor that
must land on a row the rescan has not minted: block ids are POSITIONAL, so no
id names it until the body comes back.
-}
joinLine : { a | rows : List Row, lines : List String } -> String -> Maybe Int
joinLine m id =
    Maybe.map
        (\( _, line ) ->
            if line > 1 && not (isBlank (at (line - 1) m.lines)) then
                line + 1

            else
                line
        )
        (joinAt m id)


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
    (if line > 1 && not (isBlank (at (line - 1) lines)) then
        [ "" ]

     else
        []
    )
        ++ written
        ++ (if isBlank (at line lines) then
                []

            else
                [ "" ]
           )


draftRow : Int -> String -> Row
draftRow line text =
    { blank
        | id = draftId
        , kind = Para
        , grain = Element
        , from = line
        , to = line
        , text = text
    }


{-| ROWS with ROW put in after UNDER and everything UNDER owns — past a list's
last item rather than between two — with any draft already standing taken out
first, so a second ask draws one paragraph rather than two.
-}
joined : { a | rows : List Row } -> String -> Row -> List Row
joined m under row =
    let
        kept =
            List.filter (\r -> r.id /= draftId) m.rows

        owned r =
            List.member under (ownersOf { rows = kept } r.id)

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
