port module Doc exposing (main)

{-| The materialize sheet's LEFT pane. It owns the parse, the rows, the two-axis
cursor, the grain and the delete flags; the shell keeps the keys, the edit
overlays and the writes.

The markup is the harness's and the stylesheet's: `#dlist` holds one `.de` per
stop wearing its KIND as a `d-*` class, `.dat` at point, `.dfl` on a flag,
`.dc.dc-KEY`, `.dt`/`.dl` for text, `.dg` for the unclaimed.

-}

import Array exposing (Array)
import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, style)
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Body
    exposing
        ( Cell
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
        , ownersOf
        , placeOf
        , placeOfLine
        , rowAt
        , rowById
        , rowsFrom
        , tailId
        , shown
        , undrafted
        )
import Scan exposing (Grain(..), cut, nth)



-- MODEL


type alias Link =
    { from : Int, to : Int, desc : String }


type alias Model =
    { rows : List Row
    , lines : List String

    -- THE SAME LINES, INDEXED: a row's line and a line's char offset are read per
    -- row per render, and a list walk there grew with the whole subtree.
    , arr : Array String
    , offsets : Array Int
    , at : Int
    , flags : List String
    , links : List Link
    , spanAt : Maybe Int
    , shift : Int
    , level : Int
    , titleAt : Maybe Int

    -- THE LINE A CURSOR IS OWED at the next fill: an insert's paragraph has no
    -- row until the RESCAN mints one.
    , landing : Maybe Int

    -- THE HEADER THE SERVER LIFTS: planning and the drawer ride as LISTS and
    -- their rows are synthesized, so the splice never sees them.
    , props : List ( String, String )
    , plan : List ( String, String )
    , planKeys : List String

    -- A PAIR BEING TYPED IS A ROW AND NOT A PAIR: `props' is what a flush
    -- writes to the file, so a half-typed key in it would land on disk the
    -- moment the sheet is left.  This field draws the row and nothing else.
    , draftPair : Bool

    -- The FOLDED composites, by id; every drawer starts here.
    , shut : Set String
    }


empty : Model
empty =
    Model [] [] Array.empty Array.empty 0 [] [] Nothing 0 1 Nothing Nothing [] [] [] False Set.empty


{-| A SIBLING SHARES AN OWNER, and that is the step for contents: `n'/`p' walk
the rows owned by what owns point -- a leaf its item run, an element its shelf
-- and never dive.  A LEAF'S RUN STILL ENDS AT ITS LIST'S EDGE: past the run
sit rows of other owners, and the walk stops rather than leaping shelves.
A HEADLINE IS THE EXCEPTION: it walks every visible headline in document
order, org's own cycle over the outline, contents left to `f'/`b'.
`p' IS HEADLINE-SIZED PAST ITS BODY'S EDGE: an element steps back over its
own shelf while that shelf has something above, and lands on the NEAREST
VISIBLE HEADLINE where it does not -- org's own previous-visible-heading, so
a body's first element climbs to its own headline and the TAIL past every
subtree is one press from the document's last one.
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

                -- ONE SCAN, THREE COHORTS: from a CHILD headline the walk is
                -- every visible headline in document order, org's own
                -- next-visible-heading, a folded subtree skipped whole; from
                -- a SHELF'S ELEMENT walking BACK it is that shelf AND the
                -- headlines, whichever the scan meets first; from anything
                -- else it is the rows sharing point's owner.
                -- THE ROOT IS THE READER'S EXCEPTION: the entry's own line
                -- shares its contents' cohort, so `n' steps INTO the body --
                -- though headlines walking up still land on it.  Contents
                -- are otherwise behind `f'/`b'.  The hidden fold is the
                -- headline walk's own cost, paid only on its branches.
                fits =
                    if cur.kind == Child then
                        let
                            hidden =
                                hiddenIn m
                        in
                        -- The TAIL is the outline's last stop, so the walk
                        -- down has somewhere to end past the last subtree.
                        \r ->
                            (heading r || r.id == tailId)
                                && not (Set.member r.id hidden)

                    else if by < 0 && shelved m cur then
                        let
                            hidden =
                                hiddenIn m
                        in
                        -- WHICHEVER COMES FIRST: the shelf's own previous
                        -- element while the shelf has one, else the nearest
                        -- visible HEADLINE above -- the body's own line at
                        -- its start, the deepest headline of a subtree the
                        -- row crossed.  A FOLDED SUBTREE IS ONE STEP: its
                        -- headline counts and its contents never.
                        \r ->
                            (r.owner == cur.owner || heading r)
                                && not (Set.member r.id hidden)

                    else
                        \r -> r.owner == cur.owner

                scan i =
                    if i < 0 || i >= n then
                        Nothing

                    else if Maybe.withDefault False (Maybe.map fits (nth i m.rows)) then
                        Just i

                    else
                        scan (i + by)
            in
            case scan (m.at + by) of
                Nothing ->
                    m

                Just i ->
                    { m | at = i }


{-| The one spelling of "is this row a headline?" -- the sheet's own line or
a nested child.
-}
heading : Row -> Bool
heading r =
    r.kind == Head || r.kind == Child


{-| Is ROW an element of a BODY -- the entry's own shelf or a child's -- rather
than something a composite holds?  A BODY ANSWERS TO A HEADLINE, and that is
what `p' climbs to off the shelf's edge.  A run's leaf and a drawer's pair
answer to the composite over them, so their walk still ends at ITS edge.
-}
shelved : Model -> Row -> Bool
shelved m r =
    case r.owner of
        Nothing ->
            True

        Just up ->
            Maybe.withDefault False (Maybe.map heading (rowById m up))


finer : Model -> ( Model, String )
finer m =
    case rowAt m of
        Nothing ->
            ( m, "" )

        Just r ->
            let
                kids =
                    kidsOf m r.id

                -- `f' ON A HEADLINE ENTERS THE BODY: everything is under it.
                -- The root's rows carry no owner, so its test is the count.
                entered =
                    if r.kind == Head then
                        List.length m.rows > 1

                    else
                        kids > 0
            in
            if heading r then
                if entered then
                    ( { m | at = m.at + 1 }, "grain-finer (the body)" )

                else
                    ( m, "grain-finer (an empty entry)" )

            else if kids > 0 then
                -- The first child immediately follows its parent in emission order.
                ( { m | at = m.at + 1 }
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
            case Maybe.map (placeOf m) r.owner of
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
                    ( { m | at = i }
                    , "grain-broader (" ++ word ++ ")"
                    )

                Nothing ->
                    if r.kind == Head then
                        ( m, "grain-broader (the whole entry)" )

                    else
                        -- REVERSED EXPAND-REGION at its widest step: out of a row to
                        -- its owner, out of an owner-less one to THE ENTRY'S OWN LINE.
                        ( { m | at = placeOf m "H" }
                        , "grain-broader (the headline)"
                        )



-- SPANS.  OFFSETS ARE IN CHARACTERS: the title, body and properties the lens lifts out sit
-- ABOVE the paragraphs, so a body offset past the title line is displaced by
-- one constant.


{-| Chars before LINE, the line's own newlines counted: a prefix-sum read,
since the walk it replaced ran once per row per render.
-}
charOf : Model -> Int -> Int
charOf m line =
    Maybe.withDefault 0 (Array.get line m.offsets)


offsetsOf : List String -> Array Int
offsetsOf lines =
    Array.fromList
        (List.reverse
            (List.foldl
                (\ln acc ->
                    case acc of
                        prev :: _ ->
                            (prev + String.length ln + 1) :: acc

                        [] ->
                            acc
                )
                [ 0 ]
                lines
            )
        )


{-| The span the LINKS door reads off a headline: the WHOLE SUBTREE under it,
where 'elementSpan' is a row's own extent.  The root's reach is the entry.
-}
reachSpan : Model -> Row -> Maybe ( Int, Int )
reachSpan m r =
    case ( m.spanAt, r.kind ) of
        ( Just base, Head ) ->
            Just ( base, base + m.shift + charOf m (List.length m.lines) )

        ( Just base, Child ) ->
            Just ( base + m.shift + charOf m r.from, base + m.shift + charOf m r.to )

        _ ->
            Nothing


elementSpan : Model -> Row -> Maybe ( Int, Int )
elementSpan m r =
    case m.spanAt of
        Nothing ->
            Nothing

        Just base ->
            case r.kind of
                Child ->
                    Nothing

                Meta ->
                    Nothing

                Para ->
                    if r.id == tailId then
                        -- The pane's own row: no span, so the flag and delete
                        -- doors refuse it the way they refuse a planning row.
                        Nothing

                    else
                        Just ( base + m.shift + charOf m r.from, base + m.shift + charOf m r.to )

                Head ->
                    Just ( base, base + charOf m 1 )



-- UPDATE


type Msg
    = Fill Model
    | Clear
    | Select String
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
    | DraftPair
    | UndraftPair String
    | Tab
    | AddProp String String
    | SetMeta (List ( String, String )) (List ( String, String ))
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
            told
                (reveal
                    { fresh
                        | at = landed
                        , landing = Nothing

                        -- WHAT THE READER FOLDED OR OPENED STAYS SO across the
                        -- rescan: the old answer where the id is known, the
                        -- default -- a drawer folded, a child open -- where new.
                        , shut =
                            Set.union
                                (Set.intersect model.shut (foldables fresh))
                                (Set.diff fresh.shut (foldables model))
                    }
                )

        Select id ->
            told (reveal { model | at = placeOf model id })

        Step by ->
            told (step by model)

        Finer ->
            -- `f' INTO A FOLDED DRAWER OPENS IT: a step into what is hidden shows it.
            spoke
                (finer
                    (case rowAt model of
                        Just r ->
                            if foldable model r then
                                { model | shut = Set.remove r.id model.shut }

                            else
                                model

                        Nothing ->
                            model
                    )
                )

        Broader ->
            spoke (broader model)

        Flag id ->
            -- OLDEST FIRST, the rule for every flag surface; `Listing' spells it so.
            told { model | flags = List.filter ((/=) id) model.flags ++ [ id ] }

        Unflag id ->
            told { model | flags = List.filter ((/=) id) model.flags }

        ClearFlags ->
            told { model | flags = [] }

        Tab ->
            case foldTarget model of
                Nothing ->
                    spoke ( model, "nothing folds here" )

                Just r ->
                    let
                        opened =
                            Set.member r.id model.shut
                    in
                    spoke
                        ( { model
                            | shut =
                                if opened then
                                    Set.remove r.id model.shut

                                else
                                    Set.insert r.id model.shut
                            , at = placeOf model r.id
                          }
                        , "org-cycle ("
                            ++ (if r.kind == Child then
                                    "subtree"

                                else
                                    Maybe.withDefault "drawer" r.name
                               )
                            ++ (if opened then
                                    " open)"

                                else
                                    " folded)"
                               )
                        )

        SetMeta props plan ->
            told (remeta { model | props = props, plan = plan })

        -- THE PAIR ARRIVES WHOLE -- the shell typed both halves -- so the write
        -- follows at once, and point lands on the new pair, drawer open.  THE
        -- DRAFT ROW GOES EITHER WAY: it became this pair, or the box that drew
        -- it has been told no and is shut, and a row nothing can reach would
        -- stand in the drawer until the next fill.
        -- A KEY THAT FOLDS TO A PLANNING WORD NEVER REACHES THE DRAWER: it is a
        -- planning entry wearing a property's clothes, so it is ROUTED to the
        -- planning line -- upcased, placed by the composer -- and point lands
        -- there.  ONE WRITE either way: the cargo carries both lists.
        AddProp key value ->
            if key == "" || value == "" || String.contains " " key || String.contains ":" key then
                spoke
                    ( landOn Body.drawerId (remeta { model | draftPair = False })
                    , "a property needs a key and a value"
                    )

            else
                case Body.planningKey model.planKeys key of
                    Just word ->
                        composedWith
                            (Just (Body.routedWord "the planning line" ( word, value )))
                            (landOn Body.planId
                                (remeta
                                    { model
                                        | plan = Body.setPlanning ( word, value ) model.plan
                                        , draftPair = False
                                    }
                                )
                            )

                    Nothing ->
                        let
                            fresh =
                                remeta
                                    { model
                                        | props = model.props ++ [ ( key, value ) ]
                                        , draftPair = False
                                    }
                        in
                        composedWith (Just (Body.propertyText ( key, value )))
                            { fresh
                                | at = placeOf fresh (Body.propId (List.length model.props))
                                , shut = Set.remove Body.drawerId fresh.shut
                            }

        -- Composed HERE: a deletion cannot be rebuilt out of the model it changed.
        Delete ids ->
            let
                -- A DELETED PAIR LEAVES THROUGH THE LISTS, never the splice: `d' on
                -- the drawer takes every pair, on the planning line the whole line.
                keptPlan =
                    if List.member Body.planId ids then
                        []

                    else
                        model.plan

                keptProps =
                    if List.member Body.drawerId ids then
                        []

                    else
                        List.map Tuple.second
                            (List.filter
                                (\( j, _ ) -> not (List.member (Body.propId j) ids))
                                (List.indexedMap Tuple.pair model.props)
                            )

                model_ =
                    remeta { model | plan = keptPlan, props = keptProps }

                named =
                    List.filter (\r -> List.member r.id ids) model.rows

                taken =
                    List.filter (\r -> r.kind == Para) named

                -- WHAT THE MODEL REFUSED, by name: a headline is never spliced here.
                refused =
                    List.length
                        (List.filter heading named)

                metaN =
                    List.length (List.filter (\r -> r.kind == Meta) named)

                written =
                    bodyText model (List.map .id taken)

                -- WHAT THE SPLICE ACTUALLY DROPPED, counted rather than guessed: a
                -- paragraph taken out takes the blank line under it too.
                cut =
                    List.length model.lines
                        - List.length (String.split "\n" written)

                -- THE NEXT SIBLING IS WHERE THE READER WAS WORKING, and the parent
                -- is where they end up only when the branch is emptied.  A LINE
                -- rather than an id -- the rescan mints new ones -- and a line BELOW
                -- the cut has moved up by what the cut took.
                landsOn =
                    case List.head taken of
                        Nothing ->
                            model.landing

                        Just first ->
                            let
                                kin =
                                    List.filter
                                        (\r ->
                                            r.kind == Para
                                                && r.owner == first.owner
                                                && not (List.member r.id (List.map .id taken))
                                        )
                                        model.rows
                            in
                            case List.filter (\r -> r.from > first.from) kin of
                                next :: _ ->
                                    Just (next.from - cut)

                                [] ->
                                    case List.reverse (List.filter (\r -> r.from < first.from) kin) of
                                        prev :: _ ->
                                            Just prev.from

                                        [] ->
                                            first.owner
                                                |> Maybe.andThen (rowById model)
                                                |> Maybe.map .from
                -- COMPOSED OVER THE MOVED MODEL: `at' re-derived after the rows
                -- moved, or the shell's mirror lands on the wrong row until the fill.
                done =
                    landOn (idAtRow model model.at) { model_ | landing = landsOn }
            in
            ( done
            , Cmd.batch
                [ docState (stateJSON done)
                , docTook
                    (E.object
                        ([ ( "taken", E.list E.string (List.map .id taken) )
                         , ( "refused", E.int refused )
                         , ( "meta", E.int metaN )
                         , ( "body", E.string written )
                         ]
                            ++ headerJSON done
                        )
                    )
                ]
            )

        Edit id written ->
            if Maybe.map .kind (rowById model id) == Just Meta then
                editMeta model id written

            else
                let
                    write r =
                        if r.id == id then
                            { r | text = narrowed model written }

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
            case ( insertion model id caret (narrowed model written), joinLine model id caret ) of
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

        -- `+' IN THE DRAWER DRAWS THE PAIR BEFORE IT IS WRITTEN, at the end of
        -- the drawer, which OPENS to show it -- a row typed behind a fold is no
        -- draft.  It joins no list: `props' is what a flush writes.
        DraftPair ->
            let
                fresh =
                    remeta { model | draftPair = True }
            in
            told
                { fresh
                    | at = placeOf fresh draftPairId
                    , shut = Set.remove Body.drawerId fresh.shut
                }

        -- And the same row taken away, point back on the stop `+' was pressed
        -- over: the drawer is byte-identical, having never moved.
        UndraftPair id ->
            let
                fresh =
                    remeta { model | draftPair = False }
            in
            told { fresh | at = placeOf fresh id }


{-| THE PANE IS A NARROWING: what is written stays INSIDE the materialized
subtree, so a typed headline at the root's level or above is DEMOTED to the
first child level -- editing anything outside the subtree is forbidden.
-}
narrowed : Model -> String -> String
narrowed m text =
    let
        starsAt line =
            case String.uncons line of
                Just ( '*', rest ) ->
                    1 + starsAt rest

                _ ->
                    0

        deepen line =
            let
                n =
                    starsAt line
            in
            if n > 0 && n <= m.level && String.startsWith " " (String.dropLeft n line) then
                String.repeat (m.level + 1) "*" ++ String.dropLeft n line

            else
                line
    in
    String.join "\n" (List.map deepen (String.split "\n" text))


told : Model -> ( Model, Cmd Msg )
told m =
    ( m, docState (stateJSON m) )


{-| POINT IS NEVER HIDDEN: whatever moved it, every folded drawer holding it
opens on the way.
-}
reveal : Model -> Model
reveal m =
    { m
        | shut =
            List.foldl Set.remove
                m.shut
                (List.filter (\id -> Maybe.map (foldable m) (rowById m id) == Just True)
                    (ownersOf m (idAtRow m m.at))
                )
    }


{-| The synthesized rows rebuilt after the lists moved; everything else stands.
-}
remeta : Model -> Model
remeta m =
    { m
        | rows =
            case List.filter (\r -> r.kind /= Meta) m.rows of
                head :: rest ->
                    head :: Body.metaRows m.plan m.props m.draftPair ++ rest

                [] ->
                    []
    }


{-| Point lands on ID where its row survives, on the drawer -- always drawn --
where it does not.
-}
landOn : String -> Model -> Model
landOn id m =
    { m
        | at =
            placeOf m
                (if rowById m id /= Nothing then
                    id

                 else
                    Body.drawerId
                )
    }


{-| The nearest foldable stop at or above point.
-}
foldTarget : Model -> Maybe Row
foldTarget m =
    let
        here =
            idAtRow m m.at
    in
    List.head
        (List.filter (foldable m)
            (List.filterMap (rowById m) (here :: ownersOf m here))
        )


{-| A meta row's text read back into its list: the planning line by its
keywords, a drawer line as `:KEY: value'.

A PAIR WHOSE KEY FOLDS TO A PLANNING WORD MIGRATES: some other tool minted
`:SCHEDULED:` into the drawer, and the commit takes the drawer entry off and
sets the planning one in the SAME write -- the cargo carries both lists, so
there is no half-moved pair to see between them.

-}
editMeta : Model -> String -> String -> ( Model, Cmd Msg )
editMeta m id written =
    if id == Body.planId then
        keep id (remeta { m | plan = Body.readPlanning m.planKeys written })

    else
        case ( Body.readProperty written, Body.propIndex id ) of
            ( Just ( key, value ), Just i ) ->
                let
                    -- The pair at I written over, or dropped: the migration takes
                    -- it out of the drawer, an ordinary edit rewrites it in place.
                    pairsWith now =
                        List.take i m.props ++ now ++ List.drop (i + 1) m.props
                in
                case Body.planningKey m.planKeys key of
                    Just word ->
                        composedWith
                            (Just (Body.routedWord "moved to the planning line" ( word, value )))
                            (landOn Body.planId
                                (remeta
                                    { m
                                        | props = pairsWith []
                                        , plan = Body.setPlanning ( word, value ) m.plan
                                    }
                                )
                            )

                    Nothing ->
                        keep id (remeta { m | props = pairsWith [ ( key, value ) ] })

            _ ->
                spoke ( m, "not a `:KEY: value' line — left as it was" )


keep : String -> Model -> ( Model, Cmd Msg )
keep id m =
    composed (landOn id m)


{-| A model whose rows have MOVED. BOTH ports, always — a `docBody' with no
`docState' would leave the shell's own copy a flush behind the file.
-}
composed : Model -> ( Model, Cmd Msg )
composed =
    composedWith Nothing


{-| The same write carrying SAID, the model's own word for where it landed. THE
WORD RIDES THE CARGO rather than `docSaid': two ports carry no order between
them, so a second one would race this and the caller's own wording would win
that race as often as not. `docSaid' is REFUSALS ALONE, which move no rows and
so race nothing.
-}
composedWith : Maybe String -> Model -> ( Model, Cmd Msg )
composedWith said m =
    ( m
    , Cmd.batch [ docState (stateJSON m), docBody (cargoJSON said m) ]
    )


{-| A refusal: the model is left as it was and only the word goes out.
-}
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

        -- A FRAME, not a line: what RET may not open and TAB folds.
        , ( "fold", E.bool (foldable m r) )
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
        , ( "reach"
          , case reachSpan m r of
                Just ( a, b ) ->
                    E.list E.int [ a, b ]

                Nothing ->
                    E.null
          )
        ]


stateJSON : Model -> E.Value
stateJSON m =
    E.object
        ([ ( "rows", E.list (rowJSON m) m.rows )
        , ( "at", E.int m.at )
        , ( "id", E.string (Maybe.withDefault "" (Maybe.map .id (rowAt m))) )
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
            ++ headerJSON m
        )


pairsJSON : List ( String, String ) -> E.Value
pairsJSON =
    E.list (\( k, v ) -> E.list E.string [ k, v ])


{-| The header as the wire spells it, ONCE: every write and every state push
splices these same two fields.
-}
headerJSON : Model -> List ( String, E.Value )
headerJSON m =
    [ ( "properties", pairsJSON m.props )
    , ( "planning", pairsJSON m.plan )
    ]


{-| THE COMMIT CARRIES ITS OWN CARGO: a flush reading the shell's mirrors would
race the state push for them. SAID rides with it where the model has a word of
its own for the write, and the shell echoes that in place of the wording the
caller brought.
-}
cargoJSON : Maybe String -> Model -> E.Value
cargoJSON said m =
    let
        fields =
            ( "body", E.string (bodyText m []) ) :: headerJSON m
    in
    E.object
        (case said of
            Just what ->
                ( "said", E.string what ) :: fields

            Nothing ->
                fields
        )



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
                        , arr = Array.fromList lines
                        , offsets = offsetsOf lines
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
        |> D.andThen
            (\m ->
                D.map3 (\props plan keys -> seedMeta { m | props = props, plan = plan, planKeys = keys })
                    (D.field "props" (D.list pairD))
                    (D.field "plan" (D.list pairD))
                    (D.field "planKeys" (D.list D.string))
            )


pairD : D.Decoder ( String, String )
pairD =
    D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.string)


{-| The synthesized rows put in after the headline, and EVERY DRAWER FOLDED --
the synthesized one and any the body spells raw.
-}
seedMeta : Model -> Model
seedMeta m =
    let
        seeded =
            remeta m
    in
    { seeded | shut = seedShut seeded }


idsWhere : (Row -> Bool) -> Model -> Set String
idsWhere p m =
    Set.fromList (List.filterMap (\r -> if p r then Just r.id else Nothing) m.rows)


{-| The ids TAB may fold, over whatever rows stand.
-}
foldables : Model -> Set String
foldables m =
    idsWhere (foldable m) m


{-| The ids that START folded: the drawers alone.  A child headline folds on
demand and arrives open.
-}
seedShut : Model -> Set String
seedShut m =
    idsWhere (drawer m) m


{-| A DRAWER'S FRAME, synthesized or spelled raw: what wears `d-drawer', what
starts folded, what the strip names as a reserved token.
-}
drawer : Model -> Row -> Bool
drawer m r =
    r.grain
        == Composite
        && (r.kind == Meta || Scan.drawerName (lineOf m r) /= Nothing)


{-| What TAB can fold: a drawer, and a CHILD HEADLINE, whose subtree hides
whole, org's own cycle.
-}
foldable : Model -> Row -> Bool
foldable m r =
    r.kind == Child || drawer m r


kidD : D.Decoder Body.Kid
kidD =
    D.map4 Body.Kid
        (D.field "index" D.int)
        (D.field "level" D.int)
        (D.field "line" D.int)
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

                    -- No id: a pair joins at the drawer's end and nowhere else.
                    "draftpair" ->
                        D.succeed DraftPair

                    "undraftpair" ->
                        D.map UndraftPair (D.field "id" D.string)

                    "tab" ->
                        D.succeed Tab

                    "addprop" ->
                        D.map2 AddProp (D.field "key" D.string) (D.field "value" D.string)

                    -- The stash coming back: lists edited before a detour survive it.
                    "meta" ->
                        D.map2 SetMeta
                            (D.field "props" (D.list pairD))
                            (D.field "plan" (D.list pairD))

                    _ ->
                        D.succeed Ignore
            )



-- VIEW


{-| ORG-CLEANED STARS: every star but the last a space, two spaces a level.
-}
stars : Model -> Int -> String
stars m level =
    String.repeat (max 0 (2 * (level - m.level))) " " ++ "* "


{-| THE COLUMN A ROW'S CONNECTOR STANDS IN, under the marker of the line it hangs
off -- the headline's stars for the outermost rung, the parent's bullet below that.
An attribute rather than `style`, which in 0.19 assigns `style[key]` and is ignored
for a custom property; twelve stylesheet rules said this before, one per rung.
-}
rung : Int -> Html.Attribute Msg
rung depth =
    attribute "style" ("--rail:calc(" ++ String.fromInt (2 * depth) ++ "ch - 1.5ch)")


{-| The classes a row wears. `up` lights the connector of an owner of point, and
`lvl-top` says a row is drawn at the pane's own level. The rung itself rides an
attribute — see `rung`.
-}
type alias Lit =
    { ups : List String, sib : Maybe String, owned : Set String }


{-| Point's owners, its owner, and the ids owning anything, computed ONCE per
render: `markOf' and `rowClass' read them for every row, and deriving them
there walked the rows once per row.
-}
litOf : Model -> Lit
litOf m =
    { ups = ownersOf m (idAtRow m m.at)
    , sib = Maybe.andThen .owner (rowAt m)
    , owned = Set.fromList (List.filterMap .owner m.rows)
    }


{-| Every row's owning HEADLINE, one ordered pass over the emission order: a
headline's own line sits on its PARENT's shelf, its contents on its own, so
the map answers which block a row's bar belongs to.
-}
headOf : Model -> Dict String String
headOf m =
    Tuple.second
        (List.foldl
            (\r ( stack, acc ) ->
                case r.kind of
                    Head ->
                        ( [ ( m.level, "H" ) ], acc )

                    Child ->
                        let
                            kept =
                                List.filter (\( l, _ ) -> l < r.level) stack

                            up =
                                Maybe.withDefault "H" (Maybe.map Tuple.second (List.head kept))
                        in
                        ( ( r.level, r.id ) :: kept, Dict.insert r.id up acc )

                    _ ->
                        ( stack
                        , Dict.insert r.id
                            (Maybe.withDefault "H" (Maybe.map Tuple.second (List.head stack)))
                            acc
                        )
            )
            ( [ ( m.level, "H" ) ], Dict.empty )
            m.rows
        )


{-| THE RAMP THE SPIKE'S F TAB WON WITH: every block that holds point is lit,
brightening inward — rank 0 the block point is in, a step per shelf out along
the chain, the other branches unranked and resting.
-}
spineRanks : Model -> Dict String String -> Dict String Int
spineRanks m heads =
    let
        start =
            Maybe.andThen
                (\r ->
                    if heading r then
                        Just r.id

                    else
                        Dict.get r.id heads
                )
                (rowAt m)

        chain id acc =
            if id == "H" then
                acc ++ [ "H" ]

            else
                chain (Maybe.withDefault "H" (Dict.get id heads)) (acc ++ [ id ])
    in
    case start of
        Nothing ->
            Dict.empty

        Just s ->
            Dict.fromList (List.indexedMap (\k h -> ( h, k )) (chain s []))


rowClass : Lit -> Model -> Int -> Row -> Bool -> String
rowClass lit m i r top =
    (if r.id == draftId || r.id == draftPairId then
        "de d-draft d-"

     else
        "de d-"
    )
        ++ (case r.grain of
                Leaf ->
                    -- A PAIR IS NOT NESTED: no tree, a paragraph's own face.
                    if r.kind == Meta then
                        "meta"

                    else
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
        ++ (if top then
                -- ELM MOUNTS INSIDE A WRAPPER OF ITS OWN, so `#dlist > .de' names
                -- nothing and a top-level row says so itself.  NOT `d-top': the
                -- harness reads a row's KIND off its `d-' classes.
                " lvl-top"

            else
                ""
           )
        ++ (if r.id == tailId then
                -- The empty line keeps a LINE's height, or nothing shows.
                " d-tail"

            else
                ""
           )
        ++ (if drawer m r then
                -- THE CLASS IS THE DRAWER'S, not the fold's: a child headline
                -- folds too but is no drawer, and `.d-drawer' styles frames.
                -- A drawer holding nothing is BARE, and its frame dims.
                if Set.member r.id lit.owned then
                    " d-drawer"

                else
                    " d-drawer bare"

            else
                ""
           )
        ++ markOf lit m i r


{-| `up` — the row is one of point's OWNERS: THE WAY BACK.  `sib` — the row shares
point's owner: the choice the reader is standing in. Lighting every sibling
of every ancestor lights whole levels and says nothing about it. FLAT, with no
step by distance: dimming the rest is what makes the path read, and a ramp then
said which ancestor at the cost of saying THAT.

WHAT POINT CARRIES IS NOT SPELLED HERE. A row drawn INSIDE point is what point
holds, and a composite's own children are the roots it opens, so the stylesheet
reads both off the nesting rather than Elm saying it again in a class.
-}
markOf : Lit -> Model -> Int -> Row -> String
markOf lit m i r =
    if i == m.at then
        ""

    else if List.member r.id lit.ups then
        -- THE RUN'S BARS RIDE F'S RAMP TOO: an enclosing run steps down the
        -- accent by its distance out, and only point's own run bars in `fg'.
        " up up-"
            ++ String.fromInt
                (min 2 (Maybe.withDefault 0 (indexOfIn r.id lit.ups)))

    else if r.owner /= Nothing && r.owner == lit.sib then
        -- A SIBLING IS WHAT THE READER IS CHOOSING BETWEEN, so it stays readable.
        " sib"

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
    Scan.indexWhere ((==) id) ups


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
        op =
            openerAt m r

        k =
            markerOf op (lineOf m r)

        rest =
            String.dropLeft k r.text

        opened =
            openedLen op

        box =
            String.slice opened k r.text

        -- A TICKED BOX WEARS THE DONE FACE and an empty one wears the line's, so the
        -- box is its own span: the bullet answers "a list item", the box "settled".
        mark =
            if k <= 0 then
                []

            else
                span [ class "dm" ] (markParts op (String.left opened r.text))
                    :: (if String.isEmpty box then
                            []

                        else
                            [ span
                                [ class
                                    ("dbx"
                                        ++ (if String.contains "X" box || String.contains "x" box then
                                                " on"

                                            else
                                                ""
                                           )
                                    )
                                ]
                                [ text box ]
                            ]
                       )
    in
    div [ class "dp" ]
        (mark
            ++ (case ( elementSpan m r, keyOf r ) of
                    ( Just ( a, _ ), _ ) ->
                        drawText m rest (a + k)

                    -- A PAIR'S KEY IS A RESERVED TOKEN and wears the drawer's ink,
                    -- org's `org-special-keyword' by another name.
                    ( Nothing, Just key ) ->
                        [ span [ class "dk" ] (token key)
                        , text (String.dropLeft (String.length key + 2) r.text)
                        ]

                    ( Nothing, Nothing ) ->
                        if r.id == Body.planId then
                            viewPlanning m

                        else
                            [ text rest ]
               )
        )


{-| The planning line, each keyword a reserved token the way org paints
`SCHEDULED:' -- the timestamp itself stays the line's own.
-}
viewPlanning : Model -> List (Html Msg)
viewPlanning m =
    List.concat
        (List.indexedMap
            (\i ( key, value ) ->
                [ text
                    (if i == 0 then
                        ""

                     else
                        " "
                    )
                , span [ class "dk" ]
                    [ text key, span [ class "dpunc" ] [ text ":" ] ]
                , text (" " ++ value)
                ]
            )
            m.plan
        )


{-| A reserved token drawn BY ITS LETTER: the colons dim, and the leading one
hangs into the gutter so the eye lines up on `P', never on punctuation.
-}
token : String -> List (Html Msg)
token word =
    [ span [ class "dpunc dlead" ] [ text ":" ]
    , text word
    , span [ class "dpunc" ] [ text ":" ]
    ]


keyOf : Row -> Maybe String
keyOf r =
    if r.kind == Meta && r.grain == Leaf then
        Maybe.map Tuple.first (Body.readProperty r.text)

    else
        Nothing


{-| The row's own line as org wrote it.
-}
lineOf : Model -> Row -> String
lineOf m r =
    Maybe.withDefault "" (Array.get r.from m.arr)


{-| Org's own opener for a LEAF's line; nothing when the row is not a list item.
-}
openerAt : Model -> Row -> Maybe Scan.Opener
openerAt m r =
    -- A synthesized row has NO LINE, so reading one would read the body's first.
    if r.grain /= Leaf || r.kind /= Para then
        Nothing

    else
        Scan.listOpener (lineOf m r)


{-| How many characters org spent on the indent and the bullet alone, the box after
it excluded.
-}
openedLen : Maybe Scan.Opener -> Int
openedLen =
    Maybe.withDefault 0 << Maybe.map (\o -> o.indent + String.length o.bullet)


{-| How many characters of a leaf's own line org spent on its indent, its bullet --
`-', `+', `*', `1.' or `1)' -- and the checkbox after it.
-}
markerOf : Maybe Scan.Opener -> String -> Int
markerOf op line =
    case openedLen op of
        0 ->
            0

        k ->
            k + boxLen (String.dropLeft k line)


markerLen : Model -> Row -> Int
markerLen m r =
    markerOf (openerAt m r) (lineOf m r)


{-| THE BULLETS THE TREE ALREADY DRAWS, which are the ones a stylesheet may take
away; an ordinal is content and is never one of them.
-}
stepsAside : String -> Bool
stepsAside tok =
    List.member tok [ "-", "+", "*" ]


{-| The marker's own spans, PARTITIONING the head org wrote: the indent, a steppable
bullet in a span of its own, then whatever gap follows it.
-}
markParts : Maybe Scan.Opener -> String -> List (Html Msg)
markParts op head =
    case op of
        Just o ->
            let
                tok =
                    String.slice o.indent
                        (o.indent + String.length (String.trimRight o.bullet))
                        head
            in
            if stepsAside tok then
                [ text (String.left o.indent head)
                , span [ class "dbul" ] [ text tok ]
                , text (String.dropLeft (o.indent + String.length tok) head)
                ]

            else
                [ text head ]

        Nothing ->
            [ text head ]


{-| The checkbox org may write after a bullet, with the gap that follows it. IT IS
PART OF THE MARKER: `- [X]` is one thing the reader points at, bullet and box
together.
-}
boxLen : String -> Int
boxLen rest =
    if List.member (String.left 3 rest) [ "[ ]", "[X]", "[x]", "[-]" ] then
        3 + String.length (Scan.indentOf (String.dropLeft 3 rest))

    else
        0


viewCells : Model -> Row -> List (Html Msg)
viewCells m r =
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

                        ( "title", Child, _ ) ->
                            -- THE ELLIPSIS RIDES THE TITLE: the cell grows to
                            -- fill the row, and a sibling span lands at the far
                            -- edge, detached from the words it belongs to.
                            text c.val
                                :: (if Set.member r.id m.shut then
                                        [ span [ class "dg" ] [ text " …" ] ]

                                    else
                                        []
                                   )

                        _ ->
                            [ text c.val ]
                    )
            )
            (shown r)


{-| ONE OWNER PER BYTE: a composite is drawn once with its leaves inside it, and
what no rung claims is drawn INERT (`dg`).
-}
viewKids : Lit -> Model -> Row -> Int -> Int -> Int -> ( List (Html Msg), Int )
viewKids lit m parent from at0 depth =
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
                                            viewKids lit m kid (j + 1) headAt (depth + 1)
                                    in
                                    ( own ++ deeper, jj )

                                else
                                    ( [ viewPara m kid ], j + 1 )

                        in
                        go jNext
                            kid.to
                            (out
                                ++ gap
                                ++ [ rowEl lit m j kid False [ rung depth ] inner ]
                            )

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
    go from at0 []


{-| The shelf's rail, `indent - 1.5' off the stylesheet's own arithmetic, so
every mark on a shelf agrees on its column.
-}
rail : Model -> Int -> String
rail m level =
    "--rail:calc(var(--g-doc-pad) + "
        ++ String.fromFloat (toFloat (String.length (stars m level)) - 1.5)
        ++ "ch)"


{-| A ROW ON A DEEPER SHELF indents under its own headline's FIRST LETTER --
the width its cleaned stars take, exactly the root's own geometry.
-}
inset : Model -> Row -> List (Html.Attribute Msg)
inset m r =
    if r.level > m.level then
        [ attribute "style"
            ("--g-doc-indent:"
                ++ String.fromInt (String.length (stars m r.level))
                ++ ";"
                ++ rail m r.level
            )
        ]

    else
        []


{-| ONE DOOR FOR A ROW'S DIV: the `data-id' every driver's mirror agreement
rides is owed here, so no row can be drawn without it.
-}
rowEl : Lit -> Model -> Int -> Row -> Bool -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
rowEl lit m i r top extra inner =
    div (class (rowClass lit m i r top) :: attribute "data-id" r.id :: extra) inner


{-| The rows a FOLD hides: everything owned, transitively, by a shut row.  An
owner is emitted before what it owns, so one ordered pass settles the set --
`ownersOf' per row walks the list once per row, and this runs every render.
-}
hiddenIn : Model -> Set String
hiddenIn m =
    List.foldl
        (\r acc ->
            case r.owner of
                Just o ->
                    if Set.member o m.shut || Set.member o acc then
                        Set.insert r.id acc

                    else
                        acc

                Nothing ->
                    acc
        )
        Set.empty
        m.rows


view : Model -> Html Msg
view m =
    let
        lit =
            litOf m

        hidden =
            hiddenIn m

        ranks =
            spineRanks m (headOf m)

        n =
            List.length m.rows

        -- A BLOCK IS AN ELEMENT: a headline's contents share one wrapper whose
        -- `::before' is the SPINE — continuous past margins and past deeper
        -- blocks alike, ranked on the ramp by its own class.
        blkOf id level inner =
            if List.isEmpty inner then
                []

            else
                [ div
                    [ class
                        ("blk"
                            ++ (case Dict.get id ranks of
                                    Just k ->
                                        " sp-" ++ String.fromInt (min 3 k)

                                    Nothing ->
                                        ""
                               )
                        )
                    , attribute "style" (rail m level)
                    ]
                    inner
                ]

        -- Rows until a child headline at or above LEVEL closes the block.
        go i level out =
            if i >= n then
                ( out, i )

            else
                let
                    r =
                        Maybe.withDefault blank (nth i m.rows)
                in
                if r.kind == Child && r.level <= level then
                    ( out, i )

                else if Set.member r.id hidden then
                    -- FOLDED AWAY with its owner.  What a composite holds is
                    -- transitively hidden too, so each row skips in its turn.
                    go (i + 1) level out

                else if r.kind == Child then
                    -- The headline's own line on its parent's shelf, then its
                    -- contents as a BLOCK beside it; a folded child has no
                    -- visible contents and so no block, and no spine.
                    let
                        headline =
                            rowEl lit m i r True [] (viewCells m r)

                        ( inner, j ) =
                            go (i + 1) r.level []
                    in
                    go j level (out ++ headline :: blkOf r.id r.level inner)

                else if r.grain == Composite then
                    let
                        ( inner, j ) =
                            if r.kind == Meta then
                                viewMeta lit m r (i + 1)

                            else
                                viewKids lit m r (i + 1) r.from 0

                        -- FOLDED, THE FRAME IS THE WHOLE OF IT: the opener line
                        -- and org's own ellipsis, the way org draws a shut drawer.
                        shown =
                            if Set.member r.id m.shut then
                                [ div [ class "dg" ]
                                    (opener r ++ [ text " …" ])
                                ]

                            else
                                inner
                    in
                    go j level (out ++ [ rowEl lit m i r True (inset m r) shown ])

                else
                    go (i + 1) level (out ++ [ rowEl lit m i r True (inset m r) [ viewPara m r ] ])

        -- The folded frame's own line: org's token for the synthesized drawer,
        -- the file's opener for a raw one.
        opener r =
            if r.kind == Meta then
                token "PROPERTIES"

            else
                [ text (Maybe.withDefault "" (nth r.from m.lines)) ]

        body =
            case m.rows of
                head :: _ ->
                    let
                        headline =
                            rowEl lit m 0 head True [] (viewCells m head)

                        ( inner, _ ) =
                            go 1 m.level []
                    in
                    headline :: blkOf "H" m.level inner

                [] ->
                    []
    in
    div [ class (if inList m then "focus" else "") ] (viewPath m :: body)


{-| The drawer's rows: the frame lines are the composite's own — inert, like the
lines no rung claims — and each pair is a leaf between them.  Folded, the frame
is one line with org's own ellipsis.
-}
viewMeta : Lit -> Model -> Row -> Int -> ( List (Html Msg), Int )
viewMeta lit m parent from =
    let
        walk j got =
            case nth j m.rows of
                Just kid ->
                    if kid.kind == Meta && kid.owner == Just parent.id then
                        walk (j + 1) (got ++ [ ( j, kid ) ])

                    else
                        ( got, j )

                Nothing ->
                    ( got, j )

        ( kids, next ) =
            walk from []

        leaf ( j, kid ) =
            rowEl lit m j kid False [] [ viewPara m kid ]
    in
    ( div [ class "dg" ] (token "PROPERTIES")
        :: List.map leaf kids
        ++ [ div [ class "dg" ] (token "END") ]
    , next
    )


{-| Is point INSIDE a block -- a list run, a drawer's pairs, a child's
contents? Dimming answers "which branch am I in", so it engages when there is
a branch to be in and leaves the root shelf alone.
-}
inList : Model -> Bool
inList m =
    Maybe.andThen .owner (rowAt m) /= Nothing


{-| THE WAY BACK, IN WORDS. The rails say how deep and which branch; the strip
names the same chain, so the two readings are one thing said twice. It rides the
pane's top, where the eye already is, and takes the same ramp the connectors do.
-}
viewPath : Model -> Html Msg
viewPath m =
    let
        here =
            idAtRow m m.at

        named =
            -- A CHILD IS A RUNG OF THE PATH: a headline names the way back the
            -- way a composite does, whatever its grain.
            List.map (crumb m)
                (List.filter (\r -> r.grain /= Element || r.kind == Child)
                    (List.filterMap (rowById m) (List.reverse (here :: ownersOf m here)))
                )

        -- EVERYTHING IS UNDER THE HEADLINE, so the way back starts there: the entry's
        -- own line is the root the list and the prose alike hang off.
        words =
            if Maybe.map .kind (rowAt m) == Just Head then
                [ "headline" ]

            else if List.isEmpty named then
                [ "headline"
                , if Maybe.map .kind (rowAt m) == Just Meta then
                    "planning"

                  else
                    "paragraph"
                ]

            else
                "headline" :: named

        n =
            List.length words
    in
    div [ class "dpath" ]
        (List.concat
            (List.indexedMap
                (\i w ->
                    (if i > 0 then
                        [ span [ class "dsep" ] [ text "→" ] ]

                     else
                        []
                    )
                        ++ [ span
                                [ class ("dcr cr-" ++ String.fromInt (min 3 (n - 1 - i))) ]
                                [ text w ]
                           ]
                )
                words
            )
        )


{-| What a row is called on the strip: a composite by its NAME, anything else by
its own line with the marker org wrote taken off the front.
-}
crumb : Model -> Row -> String
crumb m r =
    let
        clip s =
            if String.length s > 24 then
                String.left 23 s ++ "…"

            else
                s
    in
    if r.kind == Child then
        -- A CHILD'S CRUMB IS ITS TITLE: a headline names itself.
        let
            t =
                cellOf "title" r
        in
        clip
            (if String.isEmpty t then
                "child"

             else
                t
            )

    else if r.grain == Composite then
        -- A DRAWER'S CRUMB IS ORG'S OWN TOKEN, `:PROPERTIES:', so the strip says
        -- the reader stands in something reserved rather than in prose of theirs.
        if drawer m r then
            ":" ++ String.toUpper (Maybe.withDefault "drawer" r.name) ++ ":"

        else
            Maybe.withDefault "item" r.name

    else
        clip
            -- A PAIR'S CRUMB IS ITS KEY ALONE: the value is the line's own
            -- business, and the strip names the way back.
            (case ( r.kind, Body.readProperty r.text ) of
                ( Meta, Just ( key, _ ) ) ->
                    ":" ++ key ++ ":"

                _ ->
                    String.trim (String.dropLeft (markerLen m r) r.text)
            )



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
