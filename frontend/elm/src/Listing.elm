port module Listing exposing (main)

{-| THE SHELL'S SMALL LISTS: the link popup, the tags popup and the settings
sheet's states table — records under declared columns,
with a cursor, flags and a `/` narrow. The markup is the renderer's class for
class, because the served stylesheet is written against it. AGENTS.hs.
-}

import Browser
import Html exposing (Html, div, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Scan





type alias Badge =
    { value : String, colour : String }


type alias Column =
    { key : String, header : String, kind : String, badges : List Badge }


type alias Row =
    { id : String, cells : List ( String, String ), colour : Maybe String }


type alias Model =
    { cols : List Column
    , rows : List Row
    , at : Int
    , flags : List String
    , hint : String
    , narrow : Maybe String
    }


type Msg
    = SetRows (List Row) (Maybe String)
    | Select String
    | Step Int
    | Flag String
    | Unflag String
    | ClearFlags
    | Narrow (Maybe String)
    | Clicked String
    | Ignore





{-| SUBSTRING, CASE-FOLDED over the cells DRAWN — the producer's `substring:`
rule with no grammar: a bar, a colon and a leading `-` are literal characters.
-}
holds : String -> Model -> Row -> Bool
holds want m r =
    String.contains (String.toLower want)
        (String.toLower
            (String.join "\u{001F}" (List.map (\c -> cellOf r c.key) m.cols))
        )


{-| A narrow nobody opened is every row, and an OPEN one holding nothing is too.
-}
shown : Model -> List Row
shown m =
    case m.narrow of
        Nothing ->
            m.rows

        Just want ->
            List.filter (holds want m) m.rows


placeIn : Model -> String -> Maybe Int
placeIn m id =
    Scan.indexWhere (\r -> r.id == id) (shown m)


placeOf : Model -> String -> Int
placeOf m id =
    Maybe.withDefault m.at (placeIn m id)


idAt : Model -> String
idAt m =
    Maybe.withDefault "" (Maybe.map .id (Scan.nth m.at (shown m)))


clamp : Model -> Model
clamp m =
    { m | at = max 0 (min (List.length (shown m) - 1) m.at) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Ignore ->
            ( model, Cmd.none )

        -- The FLAGS are kept, deliberately: a caller wanting them dropped asks.
        SetRows rows landing ->
            let
                filled =
                    { model | rows = rows }
            in
            told
                (clamp
                    (case landing of
                        Just id ->
                            { filled | at = placeOf filled id }

                        Nothing ->
                            filled
                    )
                )

        Select id ->
            told (clamp { model | at = placeOf model id })

        Step by ->
            told (clamp { model | at = model.at + by })

        Flag id ->
            told { model | flags = List.filter ((/=) id) model.flags ++ [ id ] }

        Unflag id ->
            told { model | flags = List.filter ((/=) id) model.flags }

        ClearFlags ->
            told { model | flags = [] }

        -- THE CURSOR LANDS ON THE FIRST MATCH where the narrow takes its row
        -- away. FLAGS are id-keyed and untouched, as under the table's filter.
        Narrow want ->
            let
                held =
                    idAt model

                next =
                    { model | narrow = want }
            in
            told (clamp { next | at = Maybe.withDefault 0 (placeIn next held) })

        -- A click SELECTS and says so: what a surface does about it is its own.
        Clicked id ->
            let
                moved =
                    clamp { model | at = placeOf model id }
            in
            ( moved
            , Cmd.batch [ listState (stateJSON moved), listClicked (E.string id) ]
            )


told : Model -> ( Model, Cmd Msg )
told m =
    ( m, listState (stateJSON m) )





port listIn : (D.Value -> msg) -> Sub msg


{-| The shell mirrors this for readers that cannot wait a turn for a port.
-}
port listState : E.Value -> Cmd msg


port listClicked : E.Value -> Cmd msg


stateJSON : Model -> E.Value
stateJSON m =
    E.object
        [ ( "at", E.int m.at )
        , ( "id", E.string (idAt m) )
        , ( "ids", E.list E.string (List.map .id (shown m)) )
        , ( "flags", E.list E.string m.flags )

        -- What the narrow is holding, `null` where no field is up, and how many
        -- rows it is narrowing: the shell says `N of M` and reads neither list.
        , ( "narrow", Maybe.withDefault E.null (Maybe.map E.string m.narrow) )
        , ( "all", E.int (List.length m.rows) )
        ]


badgeD : D.Decoder Badge
badgeD =
    D.map2 Badge (D.field "value" D.string) (D.field "color" D.string)


columnD : D.Decoder Column
columnD =
    D.map4 Column
        (D.field "key" D.string)
        (D.field "header" D.string)
        (D.oneOf [ D.field "type" D.string, D.succeed "text" ])
        (D.oneOf [ D.field "badges" (D.list badgeD), D.succeed [] ])


cellD : D.Decoder String
cellD =
    D.oneOf
        [ D.string
        , D.map String.fromInt D.int
        , D.map String.fromFloat D.float
        , D.null ""
        ]


rowD : D.Decoder Row
rowD =
    D.map3 Row
        (D.field "id" D.string)
        (D.field "cells" (D.keyValuePairs cellD))
        (D.maybe (D.field "colour" D.string))


msgD : D.Decoder Msg
msgD =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "setRows" ->
                        D.map2 SetRows
                            (D.field "rows" (D.list rowD))
                            (D.field "at" (D.nullable D.string))

                    "select" ->
                        D.map Select (D.field "id" D.string)

                    "step" ->
                        D.map Step (D.field "by" D.int)

                    "flag" ->
                        D.map Flag (D.field "id" D.string)

                    "unflag" ->
                        D.map Unflag (D.field "id" D.string)

                    "clearFlags" ->
                        D.succeed ClearFlags

                    "narrow" ->
                        D.map Narrow (D.field "text" (D.nullable D.string))

                    _ ->
                        D.succeed Ignore
            )





{-| The renderer's own derivation: the stripe is index-borne, the rest the row's.
-}
rowClass : Model -> Int -> Row -> String
rowClass m i r =
    String.join " "
        (List.filterMap identity
            [ if modBy 2 i == 1 then
                Just "tv-alt"
              else
                Nothing
            , if List.member r.id m.flags then
                Just "tv-flagged"
              else
                Nothing
            , if i == m.at then
                Just "tv-sel"
              else
                Nothing
            ]
        )


cellOf : Row -> String -> String
cellOf r key =
    Maybe.withDefault ""
        (List.head
            (List.filterMap
                (\( k, v ) ->
                    if k == key then
                        Just v
                    else
                        Nothing
                )
                r.cells
            )
        )


{-| A BADGE CELL IS A PILL, the renderer's own markup: the palette hue tints the
ground and writes the label, so one hue carries it in either scheme.
-}
viewCell : Row -> Column -> Html Msg
viewCell r c =
    let
        val =
            cellOf r c.key

        hue =
            List.head (List.filter (\b -> b.value == val) c.badges)
    in
    case ( c.kind, hue ) of
        ( "badge", Just b ) ->
            td []
                [ span
                    [ class "tv-pill", attribute "style" ("--tv-badge:" ++ b.colour) ]
                    [ text val ]
                ]

        _ ->
            td [ style "color" (Maybe.withDefault "" r.colour) ] [ text val ]


viewRow : Model -> Int -> Row -> Html Msg
viewRow m i r =
    tr
        [ class (rowClass m i r)
        , attribute "data-id" r.id
        , onClick (Clicked r.id)
        ]
        (List.map (viewCell r) m.cols)


head : Column -> Html Msg
head c =
    th [ attribute "data-key" c.key ]
        [ span [ class "tv-hd" ]
            [ span [ class "tv-hn" ] [ text c.header ]
            , span [ class "tv-arrow" ] []
            ]
        ]


{-| THE FIELD IS THE LIST'S OWN, drawn only while a narrow is open, in the
renderer's own dress class for class — the box a reader knows from the table.
-}
bar : Model -> List (Html Msg)
bar m =
    case m.narrow of
        Nothing ->
            []

        Just want ->
            [ div [ class "tv-chips" ]
                [ div [ class "tv-filter-wrap" ]
                    [ input
                        [ class "tv-filter"
                        , type_ "search"
                        , attribute "spellcheck" "false"
                        , placeholder "narrow"
                        , value want
                        , onInput (Narrow << Just)
                        ]
                        []
                    ]
                ]
            ]


view : Model -> Html Msg
view m =
    div [ class "tv-root tv-pal" ]
        (bar m
            ++ [ div [ class "tv-scroll" ]
                    [ table [ class "tv-table" ]
                        [ thead [] [ tr [] (List.map head m.cols) ]
                        , tbody [] (List.indexedMap (viewRow m) (shown m))
                        ]
                    , div
                        [ class "tv-empty"
                        , style "display"
                            (if List.isEmpty (shown m) then
                                ""
                             else
                                "none"
                            )
                        ]
                        [ text "no rows" ]
                    ]
               , div [ class "tv-hint" ] [ text m.hint ]
               ]
        )





{-| BY HAND, since a column carries `type` and `badges` only where the caller has
them, and automatic decoding by field name would refuse the shorter shape.
-}
flagsD : D.Decoder ( List Column, String )
flagsD =
    D.map2 Tuple.pair
        (D.field "cols" (D.list columnD))
        (D.field "hint" D.string)


main : Program D.Value Model Msg
main =
    Browser.element
        { init =
            \raw ->
                let
                    ( cols, hint ) =
                        Result.withDefault ( [], "" ) (D.decodeValue flagsD raw)
                in
                ( Model cols [] 0 [] hint Nothing, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \_ -> listIn (\v -> Result.withDefault Ignore (D.decodeValue msgD v))
        }
