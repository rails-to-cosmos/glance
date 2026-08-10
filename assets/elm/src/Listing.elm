port module Listing exposing (main)

{-| THE SHELL'S SMALL LISTS, all four of them: the sheet's property panel, the
link popup, the tags popup and the settings sheet's states table.

Each is a list of RECORDS under declared columns, with a cursor, optional delete
flags and a click that selects — which is the whole widget. What a row MEANS
stays with the shell, as it always did: this draws them and says where point is.

The one list that is NOT here is the table at `#app`. That one is the renderer's
own job — hundreds of virtualized rows, filtering, sorting, marks and the crumb
trail — and it stays where it is.

The markup is the renderer's, class for class, because the served stylesheet is
written against it.

Four instances of one program: `Elm.Listing.init` per host, each with ports of
its own.

-}

import Browser
import Html exposing (Html, div, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (attribute, class, style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E



-- MODEL


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
    }


type Msg
    = SetRows (List Row) (Maybe String)
    | Select String
    | Step Int
    | Flag String
    | Unflag String
    | ClearFlags
    | Clicked String
    | Ignore



-- UPDATE


placeOf : Model -> String -> Int
placeOf m id =
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



-- PORTS


port listIn : (D.Value -> msg) -> Sub msg


{-| Where point is and what is flagged, after every change: the shell mirrors it
for the readers that cannot wait a turn for a port.
-}
port listState : E.Value -> Cmd msg


port listClicked : E.Value -> Cmd msg


stateJSON : Model -> E.Value
stateJSON m =
    E.object
        [ ( "at", E.int m.at )
        , ( "id"
          , E.string
                (Maybe.withDefault "" (Maybe.map .id (List.head (List.drop m.at m.rows))))
          )
        , ( "ids", E.list E.string (List.map .id m.rows) )
        , ( "flags", E.list E.string m.flags )
        ]


{-| A cell arrives as whatever the surface holds — the tags popup's count is a
NUMBER — and is drawn as text either way.
-}
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

                    _ ->
                        D.succeed Ignore
            )



-- VIEW


{-| The classes a row wears at display index I, the renderer's own derivation:
the zebra stripe is index-borne, the cursor and the flag are the row's.
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
ground and writes the label, so one hue carries it in either scheme. A value the
palette does not name stays plain text.
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


view : Model -> Html Msg
view m =
    div [ class "tv-root tv-pal" ]
        [ div [ class "tv-scroll" ]
            [ table [ class "tv-table" ]
                [ thead [] [ tr [] (List.map head m.cols) ]
                , tbody [] (List.indexedMap (viewRow m) m.rows)
                ]
            , div
                [ class "tv-empty"
                , style "display"
                    (if List.isEmpty m.rows then
                        ""

                     else
                        "none"
                    )
                ]
                [ text "no rows" ]
            ]
        , div [ class "tv-hint" ] [ text m.hint ]
        ]



-- MAIN


{-| FLAGS ARE DECODED BY HAND rather than by field name: a column carries `type`
and `badges` where a caller has them and neither where it does not, and the
automatic decoding would refuse the shorter shape.
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
                ( Model cols [] 0 [] hint, Cmd.none )
        , update = update
        , view = view
        , subscriptions =
            \_ -> listIn (\v -> Result.withDefault Ignore (D.decodeValue msgD v))
        }
