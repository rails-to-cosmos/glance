module Scan exposing
    ( Grain(..)
    , closers
    , Opener
    , Region
    , RegionKind(..)
    , at
    , blankAt
    , blocksIn
    , closerAt
    , cut
    , indentOf
    , isTable
    , listOpener
    , indexWhere
    , nth
    , numberAt
    , regionAt
    , takeWhileList
    )

{-| WHAT ORG TEXT IS: which line opens what, where a region runs to, and the
structure a subtree's body has.  Functions over lines and nothing else, so
`Body' and the tests can ask them without a model.
-}

import Array exposing (Array)


type Grain
    = Element
    | Composite
    | Leaf


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


{-| The closers LINES open and never write, INNERMOST FIRST, so appending them
in order closes the stack.  Text that closes itself yields none.

A VERBATIM BLOCK SUSPENDS ORG'S GRAMMAR, so nothing inside one opens anything
and only its own end closes it: a `#+begin_quote' inside a src block is text.
-}
closers : List String -> List String
closers lines =
    let
        ( stack, empty ) =
            List.foldl closerStep ( [], False ) lines
    in
    (if empty then
        [ "" ]

     else
        []
    )
        ++ List.map .closer stack


type alias Opened =
    { shut : String -> Bool, opaque : Bool, closer : String }


{-| The stack, and whether the LAST line opened what sits on top of it — which
is what makes the innermost region EMPTY and earns it a line to type on.
-}
closerStep : String -> ( List Opened, Bool ) -> ( List Opened, Bool )
closerStep line ( stack, _ ) =
    case stack of
        top :: below ->
            if top.shut line then
                ( below, False )

            else if top.opaque then
                ( stack, False )

            else
                closerPush line stack

        [] ->
            closerPush line stack


closerPush : String -> List Opened -> ( List Opened, Bool )
closerPush line stack =
    case closerOpen line of
        Just open ->
            ( open :: stack, True )

        Nothing ->
            ( stack, False )


closerOpen : String -> Maybe Opened
closerOpen line =
    case blockName line of
        Just name ->
            Just
                { shut = endsBlock name
                , opaque = verbatim name
                , closer = indentOf line ++ closerWord line name
                }

        Nothing ->
            case drawerName line of
                Just _ ->
                    Just
                        { shut = drawerEnds
                        , opaque = False
                        , closer = indentOf line ++ ":END:"
                        }

                Nothing ->
                    Nothing


{-| The opener's own spelling, turned around: `#+BEGIN_SRC' earns `#+END_SRC',
and the ARGUMENTS are dropped so a closer names only its block.
-}
closerWord : String -> String -> String
closerWord line name =
    let
        raw =
            String.trimLeft line

        begin =
            String.slice 2 7 raw
    in
    "#+"
        ++ (if begin == String.toUpper begin then
                "END"

            else
                "end"
           )
        ++ "_"
        ++ String.slice 8 (8 + String.length name) raw


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



{-| The index of the first element PRED holds for. Four callers hand-rolled this
same `indexedMap`/`filter`/`head` pipeline, differing only in the predicate.
-}
indexWhere : (a -> Bool) -> List a -> Maybe Int
indexWhere pred xs =
    List.head
        (List.filterMap
            (\( i, x ) ->
                if pred x then
                    Just i

                else
                    Nothing
            )
            (List.indexedMap Tuple.pair xs)
        )

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
