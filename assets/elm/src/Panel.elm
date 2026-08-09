port module Panel exposing (main)

{-| The materialize sheet's property panel: the key/value list beside the
document pane, drawn where a table-view mount used to be.

It owns the rows, the cursor and the delete flags, and it draws them. Everything
else about the sheet stays in the shell — the edit overlay, the flush, the keys
— which reaches this through the ports below and keeps a mirror of `state` for
its own synchronous readers.

The markup is the renderer's, class for class, because the served stylesheet is
written against it: `#mprops:not(.on) .tv-table tbody tr.tv-sel` and the rest.
-}

import Browser
import Html exposing (Html, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E



-- MODEL


type alias Row =
    { id : String
    , key : String
    , val : String
    , fixed : Bool
    }


type alias Model =
    { rows : List Row
    , at : Int
    , flags : List String
    , hint : String
    }


{-| A fresh drawer, an id the cursor should land on, a step, a commit, or one of
the flag verbs. The shell names each by a `kind` field.
-}
type Msg
    = Fill (List Row) String
    | Select String
    | Step Int
    | Add String
    | Commit String String String
    | Flag String
    | Unflag String
    | ClearFlags
    | Delete (List String)
    | Ignore



-- UPDATE


{-| Where ID sits, or the cursor's own place when no row answers to it.
-}
placeOf : String -> Model -> Int
placeOf id m =
    List.indexedMap (\i r -> ( i, r.id )) m.rows
        |> List.filter (\( _, rid ) -> rid == id)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault m.at


clamp : Model -> Model
clamp m =
    { m | at = max 0 (min (List.length m.rows - 1) m.at) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        Fill rows landing ->
            let
                filled =
                    { model | rows = rows, flags = [], at = 0 }
            in
            told (clamp { filled | at = placeOf landing filled })

        Select id ->
            told (clamp { model | at = placeOf id model })

        Step by ->
            told (clamp { model | at = model.at + by })

        Add id ->
            let
                grown =
                    { model | rows = model.rows ++ [ Row id "" "" False ] }

                landed =
                    clamp { grown | at = placeOf id grown }
            in
            ( landed
            , Cmd.batch
                [ panelState (stateJSON landed)
                , panelOpen (rowJSON (Row id "" "" False))
                ]
            )

        Commit id key val ->
            let
                write r =
                    if r.id /= id then
                        r

                    else
                        { r | key = if r.fixed then r.key else key, val = val }
            in
            told { model | rows = List.map write model.rows }

        Flag id ->
            told { model | flags = id :: List.filter ((/=) id) model.flags }

        Unflag id ->
            told { model | flags = List.filter ((/=) id) model.flags }

        ClearFlags ->
            told { model | flags = [] }

        -- A planning row is CLEARED and stays, since an empty value is already
        -- how an entry is absent; a property is DROPPED. Which is which is the
        -- row's own `fixed', and the shell is told what it cleared so its echo
        -- can name the keywords.
        Delete ids ->
            let
                going r =
                    List.member r.id ids

                cleared =
                    List.filter (\r -> going r && r.fixed) model.rows

                kept r =
                    r.fixed || not (going r)

                blank r =
                    if going r && r.fixed then
                        { r | val = "" }

                    else
                        r

                left =
                    List.filter kept (List.map blank model.rows)

                after =
                    clamp
                        { model
                            | rows = left
                            , flags = List.filter (\f -> not (List.member f ids)) model.flags
                        }
            in
            ( after
            , Cmd.batch
                [ panelState (stateJSON after)
                , panelTook (E.list E.string (List.map .key cleared))
                ]
            )


told : Model -> ( Model, Cmd Msg )
told m =
    ( m, panelState (stateJSON m) )



-- PORTS


port panelIn : (D.Value -> msg) -> Sub msg


{-| The whole model, after every change: the shell mirrors it for the readers
that cannot wait a turn for a port.
-}
port panelState : E.Value -> Cmd msg


{-| A row the shell should lay its edit overlay over — `+` and nothing else.
-}
port panelOpen : E.Value -> Cmd msg


{-| The keys of the planning rows a delete CLEARED, for the shell's echo.
-}
port panelTook : E.Value -> Cmd msg


rowJSON : Row -> E.Value
rowJSON r =
    E.object
        [ ( "id", E.string r.id )
        , ( "key", E.string r.key )
        , ( "val", E.string r.val )
        , ( "fixed", E.bool r.fixed )
        ]


stateJSON : Model -> E.Value
stateJSON m =
    E.object
        [ ( "rows", E.list rowJSON m.rows )
        , ( "at", E.int m.at )
        , ( "id", E.string (Maybe.withDefault "" (Maybe.map .id (rowAt m))) )
        , ( "flags", E.list E.string m.flags )
        ]


rowAt : Model -> Maybe Row
rowAt m =
    List.head (List.drop m.at m.rows)


rowD : D.Decoder Row
rowD =
    D.map4 Row
        (D.field "id" D.string)
        (D.field "key" D.string)
        (D.field "val" D.string)
        (D.field "fixed" D.bool)


msgD : D.Decoder Msg
msgD =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "fill" ->
                        D.map2 Fill (D.field "rows" (D.list rowD)) (D.field "at" D.string)

                    "select" ->
                        D.map Select (D.field "id" D.string)

                    "step" ->
                        D.map Step (D.field "by" D.int)

                    "add" ->
                        D.map Add (D.field "id" D.string)

                    "commit" ->
                        D.map3 Commit
                            (D.field "id" D.string)
                            (D.field "key" D.string)
                            (D.field "val" D.string)

                    "flag" ->
                        D.map Flag (D.field "id" D.string)

                    "unflag" ->
                        D.map Unflag (D.field "id" D.string)

                    "clearFlags" ->
                        D.succeed ClearFlags

                    "delete" ->
                        D.map Delete (D.field "ids" (D.list D.string))

                    _ ->
                        D.succeed Ignore
            )



-- VIEW


{-| The classes a row wears at display index I, the renderer's own derivation:
the zebra stripe is index-borne, and the cursor and the flag are the row's.
-}
rowClass : Model -> Int -> Row -> String
rowClass m i r =
    String.join " "
        (List.filterMap identity
            [ if modBy 2 i == 1 then Just "tv-alt" else Nothing
            , if List.member r.id m.flags then Just "tv-flagged" else Nothing
            , if i == m.at then Just "tv-sel" else Nothing
            ]
        )


viewRow : Model -> Int -> Row -> Html Msg
viewRow m i r =
    tr [ class (rowClass m i r), attribute "data-id" r.id, onClick (Select r.id) ]
        [ td [] [ text r.key ]
        , td [] [ text r.val ]
        ]


head : String -> Html Msg
head word =
    th []
        [ span [ class "tv-hd" ]
            [ span [ class "tv-hn" ] [ text word ]
            , span [ class "tv-arrow" ] []
            ]
        ]


view : Model -> Html Msg
view m =
    div [ class "tv-root tv-pal" ]
        [ div [ class "tv-scroll" ]
            [ table [ class "tv-table" ]
                [ thead [] [ tr [] [ head "Key", head "Value" ] ]
                , tbody [] (List.indexedMap (viewRow m) m.rows)
                ]
            , div
                [ class "tv-empty"
                , style "display" (if List.isEmpty m.rows then "" else "none")
                ]
                [ text "no rows" ]
            ]
        , div [ class "tv-hint" ] [ text m.hint ]
        ]



-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init =
            \hint ->
                ( Model [] 0 [] hint, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \_ -> panelIn (\v -> Result.withDefault Ignore (D.decodeValue msgD v))
        }
