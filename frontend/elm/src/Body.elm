module Body exposing
    ( Cell
    , Kid
    , Kind(..)
    , Row
    , blank
    , bodyText
    , caretIn
    , draftId
    , draftPairId
    , drafted
    , insertion
    , joinLine
    , joinWord
    , cellOf
    , kidsOf
    , kindWord
    , metaRows
    , drawerId
    , planId
    , propId
    , propIndex
    , ownersOf
    , markerFor
    , planEntries
    , planningKey
    , planningText
    , propertyText
    , readPlanning
    , readProperty
    , routedWord
    , setPlanning
    , placeAtLine
    , placeOf
    , placeOfLine
    , rowAt
    , rowById
    , rowsFrom
    , tailId
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
        closerAt, cut, indentOf, isTable, listOpener, nth, numberAt,
        regionAt, takeWhileList)


type Kind
    = Head
    | Para
    | Child
    | Meta


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


type alias Kid =
    { index : Int, level : Int, line : Int, cells : List Cell }


-- THE SYNTHESIZED IDS, minted here and parsed nowhere by hand.


planId : String
planId =
    "PLN"


drawerId : String
drawerId =
    "PR"


tailId : String
tailId =
    "T"


propId : Int -> String
propId n =
    drawerId ++ String.fromInt n


{-| The row a pair is typed over before it is written.  NOT a `propId`: it is
no index, so `propIndex` reads it back as nothing and no delete can name it.
-}
draftPairId : String
draftPairId =
    drawerId ++ "D"


propIndex : String -> Maybe Int
propIndex id =
    String.toInt (String.dropLeft (String.length drawerId) id)


{-| The rows a filled pane holds: the headline, the entry's own blocks, then
EVERY DESCENDANT WHOLE -- its headline row, its blocks under it -- so the pane
is the subtree rather than one shelf of it, and last THE TAIL: one empty line
past everything, hidden until the walk reaches it -- the door to a paragraph
after a document that ends in something `+' owns.  A child's contents run to the next child's line; a
child OWNS its blocks, and a deeper child its shallower one, so the walk reads
nesting off ownership exactly as it does inside a list.
-}
rowsFrom : List String -> Int -> List Cell -> List Kid -> List Row
rowsFrom lines own headCells kids =
    let
        head =
            { blank | id = "H", kind = Head, grain = Element, cells = headCells }

        arr =
            Array.fromList lines

        -- LVL is the OWNING HEADLINE's level: a child's contents indent under the
        -- child's own first letter, so the row carries whose shelf it stands on.
        rowsIn prefix owner lvl a b =
            let
                blocks =
                    Scan.blocksInRange arr a b

                idAt k =
                    prefix ++ "B" ++ String.fromInt k
            in
            List.indexedMap
                (\i b_ ->
                    let
                        held =
                            cut lines b_.from b_.to
                    in
                    { blank
                        | id = idAt i
                        , kind = Para
                        , grain = b_.grain
                        , name = b_.name
                        , level = lvl
                        , owner =
                            case b_.up of
                                Just k ->
                                    Just (idAt k)

                                Nothing ->
                                    owner
                        , from = b_.from
                        , to = b_.to
                        , text = held
                        , was = held
                    }
                )
                blocks

        body =
            rowsIn "" Nothing 1 0 own

        owned r =
            let
                stop =
                    ownEnd body r
            in
            if stop == r.to then
                r
            else
                { r | text = cut lines r.from stop, was = cut lines r.from stop }

        descend seen ks out =
            case ks of
                [] ->
                    out

                k :: rest ->
                    let
                        stop =
                            case rest of
                                next :: _ ->
                                    next.line

                                [] ->
                                    List.length lines

                        above =
                            List.filter (\( l, _ ) -> l < k.level) seen

                        cid =
                            "C" ++ String.fromInt k.index

                        row =
                            { blank
                                | id = cid
                                , kind = Child
                                , grain = Element
                                , index = k.index
                                , level = k.level
                                , cells = k.cells
                                , owner = Maybe.map Tuple.second (List.head above)
                                , from = k.line
                                , to = stop
                            }
                    in
                    descend (( k.level, cid ) :: above)
                        rest
                        (out ++ row :: rowsIn (cid ++ ":") (Just cid) k.level (k.line + 1) stop)
        -- Zero-width and `alone', so an edit SPLICES a fresh paragraph at the
        -- end and an untouched tail never reaches `bodyText'.
        tail =
            { blank
                | id = tailId
                , from = List.length lines
                , to = List.length lines
                , alone = True
            }
    in
    (head :: List.map owned body) ++ descend [] kids [] ++ [ tail ]



-- THE HEADER THE SERVER LIFTS, DRAWN BACK: planning and the properties drawer
-- arrive as LISTS beside the body, so their rows are SYNTHESIZED -- no span, no
-- part in the splice ('bodyText' walks 'Para' alone), edited as lists.


{-| The planning line and the properties drawer as rows: planning one leaf-line
element when any pair exists, the drawer a composite (id `PR') over one leaf per
pair.  The drawer is always drawn -- `+' needs a place to land.  DRAFTING draws
one more leaf, empty, where the pair being typed will stand: a row and not a
PAIR, since the drawer's list is what a flush writes.  ENTRIES is the line as the
pane DRAWS it (`planEntries'), so it stands where the entry does not exist yet
and the row and the HTML paint one list.
-}
metaRows :
    { entries : List ( String, String )
    , props : List ( String, String )
    , drafting : Bool
    }
    -> List Row
metaRows { entries, props, drafting } =
    let
        planning =
            if List.isEmpty entries then
                []
            else
                [ { blank
                    | id = planId
                    , kind = Meta
                    , grain = Element
                    , text = planningText entries
                    , was = planningText entries
                  }
                ]

        drawer =
            { blank
                | id = drawerId
                , kind = Meta
                , grain = Composite
                , name = Just "properties"
            }

        pair i ( key, value ) =
            { blank
                | id = propId i
                , kind = Meta
                , grain = Leaf
                , owner = Just drawerId
                , text = propertyText ( key, value )
                , was = propertyText ( key, value )
            }

        drafts =
            if drafting then
                [ { blank
                    | id = draftPairId
                    , kind = Meta
                    , grain = Leaf
                    , owner = Just drawerId
                  }
                ]
            else
                []
    in
    planning ++ drawer :: List.indexedMap pair props ++ drafts


planningText : List ( String, String ) -> String
planningText plan =
    String.join " " (List.map (\( k, v ) -> k ++ ": " ++ v) plan)


{-| The planning entries as the pane DRAWS them: the model's own, and after them
the keyword a summoned widget has ghosted in, valueless, so a value the entry has
not got still has a slot to stand in.  It lands at the END, where `setPlanning'
and the server's own composer put an entry the line did not already hold.
A GHOSTED KEYWORD IS NO ENTRY: `plan' is what a flush writes, and this list is
never that.
-}
planEntries : List ( String, String ) -> Maybe String -> List ( String, String )
planEntries plan summoned =
    case summoned of
        Just key ->
            if List.any (\( k, _ ) -> k == key) plan then
                plan
            else
                plan ++ [ ( key, "" ) ]

        Nothing ->
            plan


{-| KEY as one of KEYWORDS, the case folded away, or nothing. A drawer key that
folds to a planning word is a PLANNING ENTRY WEARING A PROPERTY'S CLOTHES: it
belongs on the planning line, upcased, and never in the drawer.
-}
planningKey : List String -> String -> Maybe String
planningKey keywords key =
    let
        up =
            String.toUpper key
    in
    if List.member up keywords then
        Just up
    else
        Nothing


{-| KEY set to VALUE on the planning line: an entry already there is replaced
where it stands and a fresh one lands at the end, which is where the server's
own composer writes an entry it did not already hold. AN EMPTY VALUE CLEARS
THE ENTRY, org's own way and `readPlanning''s -- a keyword left valueless is
no planning entry.
-}
setPlanning : ( String, String ) -> List ( String, String ) -> List ( String, String )
setPlanning ( key, value ) plan =
    if value == "" then
        List.filter (\( k, _ ) -> k /= key) plan
    else if List.any (\( k, _ ) -> k == key) plan then
        List.map
            (\p ->
                if Tuple.first p == key then
                    ( key, value )
                else
                    p
            )
            plan
    else
        plan ++ [ ( key, value ) ]


{-| What a write the model ROUTED to the planning line says of itself. LANDING
is the caller's half of the sentence -- a pair typed fresh in the box lands on
`the planning line', one lifted out of the drawer is `moved to' it -- and AN
EMPTY VALUE CLEARED the entry instead, which says so rather than naming a
landing. THE THREE ROUTING WORDINGS LIVE HERE, so the shell's own assertions
have one place to follow.
-}
routedWord : String -> ( String, String ) -> String
routedWord landing ( key, value ) =
    if value == "" then
        key ++ " cleared, and the drawer pair with it"
    else
        planningText [ ( key, value ) ] ++ " — " ++ landing


propertyText : ( String, String ) -> String
propertyText ( key, value ) =
    ":" ++ key ++ ": " ++ value


{-| A property line read back: `:KEY: value'.  A line that opens no drawer key
is refused, so a typo never writes a silent prose line into the drawer.
-}
readProperty : String -> Maybe ( String, String )
readProperty line =
    let
        t =
            String.trim line
    in
    if String.startsWith ":" t then
        case String.indexes ":" (String.dropLeft 1 t) of
            close :: _ ->
                let
                    key =
                        String.slice 1 (close + 1) t
                in
                if key == "" || String.contains " " key then
                    Nothing
                else
                    Just ( key, String.trim (String.dropLeft (close + 2) t) )

            [] ->
                Nothing
    else
        Nothing


{-| The planning line read back: pairs cut at each keyword the line spells, in
the order it spells them.  A keyword left VALUELESS is dropped, which is how an
edit clears one.
-}
readPlanning : List String -> String -> List ( String, String )
readPlanning keywords line =
    let
        marks =
            List.sortBy Tuple.first
                (List.concatMap
                    (\k ->
                        List.map (\i -> ( i, k ))
                            (String.indexes (k ++ ":") line)
                    )
                    keywords
                )

        slice ( i, k ) rest =
            let
                stop =
                    Maybe.withDefault (String.length line)
                        (Maybe.map Tuple.first (List.head rest))
            in
            ( k
            , String.trim
                (String.slice (i + String.length k + 1) stop line)
            )

        go seen =
            case seen of
                [] ->
                    []

                mark :: rest ->
                    slice mark rest :: go rest
    in
    List.filter (\( _, v ) -> v /= "") (go marks)



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

        -- Nothing spoke on most pushes, and the owner walk is the pane's cost.
        silenced r =
            not (List.isEmpty spoken)
                && List.any (\o -> List.member o spoken) (ownersOf m r.id)

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

                Meta ->
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
    Maybe.withDefault m.at
        (Scan.indexWhere (\r -> r.kind == Para && r.from == line) m.rows)


{-| The place a FILL'S OWN LANDING names: the innermost block COVERING that
line, the first block at or past it where the line itself holds nothing, and the
last stop where the line runs past every block.

`placeOfLine' is exact because an insert knows the line its paragraph opens on;
this reading is not, because a capture's `%?' EXPANDS TO NOTHING -- the line
point is owed is usually blank, and the honest answer there is the stop that
follows it. THE TAIL IS A BLOCK LIKE ANY OTHER here, which is what makes "point
after everything" reachable.
-}
placeAtLine : { a | rows : List Row, at : Int } -> Int -> Int
placeAtLine m line =
    let
        stops =
            List.filter (\( _, r ) -> r.kind == Para)
                (List.indexedMap Tuple.pair m.rows)

        -- FOLDED FORWARD, so a nested item wins over the run that holds it.
        covering =
            List.foldl
                (\( i, r ) held ->
                    if r.from <= line && line < r.to then
                        Just i
                    else
                        held
                )
                Nothing
                stops

        after =
            List.head (List.filter (\( _, r ) -> r.from >= line) stops)

        last =
            List.head (List.reverse stops)
    in
    case ( covering, after ) of
        ( Just i, _ ) ->
            i

        ( Nothing, Just ( i, _ ) ) ->
            i

        ( Nothing, Nothing ) ->
            Maybe.withDefault m.at (Maybe.map Tuple.first last)


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
    Maybe.withDefault m.at (Scan.indexWhere (\r -> r.id == id) m.rows)


shown : Row -> List Cell
shown r =
    List.filter (\c -> c.val /= "") r.cells


{-| A head never carries an owner, so ownership alone counts a row's kids.
-}
kidsOf : { a | rows : List Row } -> String -> Int
kidsOf m id =
    List.length (List.filter (\r -> r.owner == Just id) m.rows)


{-| The value a row's cell carries under KEY, empty when it carries none.
-}
cellOf : String -> Row -> String
cellOf key r =
    Maybe.withDefault ""
        (Maybe.map .val (List.head (List.filter (\c -> c.key == key) r.cells)))


kindWord : Kind -> String
kindWord k =
    case k of
        Head ->
            "head"

        Para ->
            "para"

        Child ->
            "child"

        Meta ->
            "meta"
