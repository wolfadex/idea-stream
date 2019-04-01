port module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Css
import Css.Transitions as Transitions
import Debug
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Html.Styled.Keyed as KeyedHtml
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Task
import Time exposing (Month(..), Posix, Zone)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



---- TYPES ----


type alias Model =
    { oldThoughts : List ( Thought, Posix )
    , currentThought : Maybe ( Thought, Posix )
    , attemptPurge : Bool
    , showMenu : Bool
    , showAbout : Bool
    , currentZone : Maybe Zone
    , themeColor : Float
    }


type alias Thought =
    String


type Msg
    = NoOp
    | CreateThought
    | StartThought Posix
    | UpdateThought String
    | UpdateThoughtTime Posix
    | TryStoringThought Posix
    | StoreAndCreateThought
    | StoreOrCreateThought
    | AttemptPurge
    | CancelPurge
    | ExecutePurge
    | ShowMenu
    | HideMenu
    | ShowAbout
    | HideAbout
    | SetZone Zone
    | UpdateThemeColor Float



---- INIT ----


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        ( priorThoughts, now ) =
            Debug.log "init" <| flagsDecoder flags
    in
    ( { oldThoughts = priorThoughts
      , currentThought = Nothing
      , attemptPurge = False
      , showMenu = False
      , showAbout = False
      , currentZone = Nothing
      , themeColor = now
      }
    , Cmd.batch
        [ focusNewThoughtButton
        , Task.perform SetZone Time.here
        ]
    )


flagsDecoder : Value -> ( List ( Thought, Posix ), Float )
flagsDecoder val =
    case Decode.decodeValue decodeFlags val of
        Ok thoughts ->
            thoughts

        Err _ ->
            ( [], 0 )


decodeFlags : Decoder ( List ( Thought, Posix ), Float )
decodeFlags =
    Decode.map2
        (\thoughts now -> ( Maybe.withDefault [] thoughts, now |> modBy 360 |> toFloat ))
        (Decode.maybe (Decode.field "priorThoughts" (Decode.list decodeThought)))
        (Decode.field "now" Decode.int)


decodeThought : Decoder ( Thought, Posix )
decodeThought =
    Decode.map2
        (\thought time ->
            ( thought, Time.millisToPosix time )
        )
        (Decode.field "thought" Decode.string)
        (Decode.field "time" Decode.int)



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every (15 * 1000) TryStoringThought
        , onKeyDown decodeKeyDown
        , onAnimationFrameDelta UpdateThemeColor
        ]


decodeKeyDown : Decoder Msg
decodeKeyDown =
    Decode.map4
        keyDownToMsg
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "repeat" Decode.bool)


keyDownToMsg : String -> Bool -> Bool -> Bool -> Msg
keyDownToMsg key ctrlDown shiftDown isRepeat =
    if isRepeat then
        NoOp

    else
        case ( key, ctrlDown, shiftDown ) of
            ( "Enter", True, False ) ->
                StoreAndCreateThought

            ( "Enter", False, True ) ->
                StoreOrCreateThought

            _ ->
                NoOp



---- PORTS ----
-- OUTGOING


port writeThoughts : Value -> Cmd msg


writeThoughtsToDisk : List ( Thought, Posix ) -> Cmd msg
writeThoughtsToDisk thoughts =
    writeThoughts <| Encode.list encodeThought thoughts


encodeThought : ( Thought, Posix ) -> Value
encodeThought ( thought, time ) =
    Encode.object
        [ ( "thought", Encode.string thought )
        , ( "time", Encode.int <| Time.posixToMillis time )
        ]


port purgeThoughts : () -> Cmd msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateThemeColor delta ->
            let
                nextColor =
                    model.themeColor + delta / 1000
            in
            ( { model
                | themeColor =
                    if nextColor > 360 then
                        nextColor - 360

                    else
                        nextColor
              }
            , Cmd.none
            )

        SetZone zone ->
            ( { model | currentZone = Just zone }, Cmd.none )

        ShowAbout ->
            ( { model | showAbout = True, showMenu = False }
            , focusElement "hideAboutButton"
            )

        HideAbout ->
            ( { model | showAbout = False }, focusNewThoughtButton )

        AttemptPurge ->
            ( { model | attemptPurge = True, showMenu = False }
            , focusElement "cancelPurgeButton"
            )

        CancelPurge ->
            ( { model | attemptPurge = False }, focusNewThoughtButton )

        ExecutePurge ->
            ( { model | attemptPurge = False, oldThoughts = [], currentThought = Nothing }
            , Cmd.batch
                [ purgeThoughts ()
                , focusNewThoughtButton
                ]
            )

        ShowMenu ->
            ( { model | showMenu = True }, Cmd.none )

        HideMenu ->
            ( { model | showMenu = False }, Cmd.none )

        CreateThought ->
            ( model
            , Task.perform StartThought Time.now
            )

        StartThought time ->
            ( { model
                | currentThought = Just ( "", time )
                , showMenu = False
                , showAbout = False
                , attemptPurge = False
              }
            , focusElement "thoughtBox"
            )

        UpdateThought newContent ->
            case model.currentThought of
                Nothing ->
                    ( model, Cmd.none )

                Just ( _, t ) ->
                    ( { model | currentThought = Just ( newContent, t ) }
                    , Task.perform UpdateThoughtTime Time.now
                    )

        UpdateThoughtTime t ->
            case model.currentThought of
                Nothing ->
                    ( model, Cmd.none )

                Just ( thought, _ ) ->
                    ( { model | currentThought = Just ( thought, t ) }
                    , Cmd.none
                    )

        TryStoringThought now ->
            case model.currentThought of
                Nothing ->
                    ( model, Cmd.none )

                Just ( _, t ) ->
                    if Time.posixToMillis now - Time.posixToMillis t > (5 * 60 * 1000) then
                        let
                            nextModel =
                                storeThought model
                        in
                        ( nextModel
                        , Cmd.batch
                            [ focusNewThoughtButton
                            , writeThoughtsToDisk nextModel.oldThoughts
                            ]
                        )

                    else
                        ( model, Cmd.none )

        StoreAndCreateThought ->
            let
                nextModel =
                    case model.currentThought of
                        Nothing ->
                            model

                        Just _ ->
                            storeThought model
            in
            ( nextModel
            , Cmd.batch
                [ Task.perform (\_ -> CreateThought) (Task.succeed ())
                , writeThoughtsToDisk nextModel.oldThoughts
                ]
            )

        StoreOrCreateThought ->
            case model.currentThought of
                Nothing ->
                    ( model, Task.perform (\_ -> CreateThought) (Task.succeed ()) )

                Just _ ->
                    let
                        nextModel =
                            storeThought model
                    in
                    ( nextModel
                    , Cmd.batch
                        [ focusNewThoughtButton
                        , writeThoughtsToDisk nextModel.oldThoughts
                        ]
                    )


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


focusNewThoughtButton : Cmd Msg
focusNewThoughtButton =
    focusElement "newThoughtButton"


storeThought : Model -> Model
storeThought model =
    case model.currentThought of
        Nothing ->
            model

        Just ( thought, t ) ->
            let
                trimmedThought =
                    String.trim thought
            in
            if String.length trimmedThought < 1 then
                { model | currentThought = Nothing }

            else
                { model
                    | currentThought = Nothing
                    , oldThoughts = ( trimmedThought, t ) :: model.oldThoughts
                }



---- VIEW ----


darkColor : Css.Color
darkColor =
    Css.rgb 50 10 10


lightColor : Float -> Css.Color
lightColor themeColor =
    Css.hsl themeColor 1 0.89



-- Css.rgb 255 200 200


view : Model -> Document Msg
view ({ showMenu, themeColor } as model) =
    { title = "Idea Stream"
    , body =
        [ Html.toUnstyled <|
            viewIdeaStream model
        , Html.toUnstyled <|
            viewMenuButton
                { open = showMenu
                , openHandler = ShowMenu
                , closeHandler = HideMenu
                , color = lightColor themeColor
                }
        , Html.toUnstyled <|
            viewMenu model
        , Html.toUnstyled <|
            viewPurgeAttempt model
        , Html.toUnstyled <|
            viewAbout model
        ]
    }


viewModal : Float -> List (Html Msg) -> Html Msg
viewModal themeColor children =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top <| Css.px 0
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.backgroundColor <| Css.rgba 0 0 0 0.5
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.fontFamily Css.sansSerif
            , Css.fontSize <| Css.rem 1.2
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.backgroundColor <| lightColor themeColor
                , Css.padding <| Css.rem 1
                , Css.maxWidth <| Css.rem 40
                ]
            ]
            children
        ]


viewAbout : Model -> Html Msg
viewAbout { themeColor, showAbout } =
    if showAbout then
        viewModal themeColor
            [ Html.span
                []
                [ Html.text """A simple app for storing thoughts.
All thoughts are stored in localStorage (in your browser). There's a button to purge all imformation, because the data is yours and only yours.

If you'd like to see the code, it can be found at """
                , Html.a
                    [ Attrs.href "https://github.com/wolfadex/idea-stream"
                    , Attrs.target "_blank"
                    ]
                    [ Html.text "idea-stream on GitHub" ]
                , Html.text "."
                ]
            , Html.br [] []
            , Html.br [] []
            , Html.section
                [ Attrs.css
                    [ Css.displayFlex
                    , Css.justifyContent Css.flexEnd
                    ]
                ]
                [ Html.button
                    [ Attrs.css
                        (defaultButtonStyle themeColor)
                    , Events.onClick HideAbout
                    , Attrs.id "hideAboutButton"
                    ]
                    [ Html.text "Back to Your Ideas" ]
                ]
            ]

    else
        Html.text ""


viewMenuButton : { open : Bool, openHandler : Msg, closeHandler : Msg, color : Css.Color } -> Html Msg
viewMenuButton { open, openHandler, closeHandler, color } =
    Html.button
        [ Events.onClick <|
            if open then
                closeHandler

            else
                openHandler
        , Attrs.css
            [ Css.position Css.absolute
            , Css.bottom <| Css.rem 1
            , Css.right <| Css.rem 1
            , Css.backgroundColor darkColor
            , Css.border <| Css.px 0
            , Css.cursor Css.pointer
            , Css.borderRadius <| Css.rem 50
            , Css.padding <| Css.rem 1
            , Css.overflow Css.visible
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.display Css.inlineBlock
                , Css.width <| Css.px 35
                , Css.height <| Css.px 35
                ]
            ]
            [ Html.div
                [ Attrs.css
                    [ Css.width <| Css.px 35
                    , Css.height <| Css.px 5
                    , Css.backgroundColor color
                    , Css.margin2 (Css.px 6) (Css.px 0)
                    , Css.borderRadius <| Css.px 10

                    -- This bar only
                    , Css.transforms <|
                        if open then
                            [ Css.rotate <| Css.deg -45
                            , Css.translate2 (Css.px -9) (Css.px 6)
                            ]

                        else
                            [ Css.rotate <| Css.deg 0
                            , Css.translate2 (Css.px 0) (Css.px 0)
                            ]
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.px 35
                    , Css.height <| Css.px 5
                    , Css.backgroundColor color
                    , Css.margin2 (Css.px 6) (Css.px 0)
                    , Css.borderRadius <| Css.px 10

                    -- This bar only
                    , Css.opacity <|
                        Css.num <|
                            if open then
                                0

                            else
                                1
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.px 35
                    , Css.height <| Css.px 5
                    , Css.backgroundColor color
                    , Css.margin2 (Css.px 6) (Css.px 0)
                    , Css.borderRadius <| Css.px 10

                    -- This bar only
                    , Css.transforms <|
                        if open then
                            [ Css.rotate <| Css.deg 45
                            , Css.translate2 (Css.px -8) (Css.px -8)
                            ]

                        else
                            [ Css.rotate <| Css.deg 0
                            , Css.translate2 (Css.px 0) (Css.px 0)
                            ]
                    ]
                ]
                []
            ]
        ]


viewMenu : Model -> Html Msg
viewMenu { showMenu, themeColor } =
    if showMenu then
        Html.div
            [ Attrs.css
                [ Css.position Css.absolute
                , Css.bottom <| Css.rem 7
                , Css.right <| Css.rem 1
                -- , Css.width <| Css.rem 15
                , Css.border3 (Css.px 1) Css.solid (Css.rgb 0 0 0)
                , Css.backgroundColor <| lightColor themeColor
                ]
            ]
            [ Html.ul
                [ Attrs.css
                    [ Css.margin <| Css.rem 0
                    , Css.listStyle Css.none
                    , Css.padding <| Css.rem 1
                    ]
                ]
                [ Html.li
                    []
                    [ Html.button
                        [ Attrs.css
                            (defaultButtonStyle themeColor)
                        , Events.onClick AttemptPurge
                        ]
                        [ Html.text "Purge?" ]
                    ]
                , Html.li
                    [ Attrs.css
                        [ Css.marginTop <| Css.rem 1 ]
                    ]
                    [ Html.button
                        [ Attrs.css
                            (defaultButtonStyle themeColor)
                        , Events.onClick ShowAbout
                        ]
                        [ Html.text "About" ]
                    ]
                ]
            ]

    else
        Html.text ""


viewPurgeAttempt : Model -> Html Msg
viewPurgeAttempt { attemptPurge, themeColor } =
    if attemptPurge then
        viewModal themeColor
            [ Html.p
                [ Attrs.css
                    [ Css.whiteSpace Css.preWrap ]
                ]
                [ Html.span
                    []
                    [ Html.text "Would you like to purge all of your memories?"
                    ]
                , Html.br [] []
                , Html.span
                    []
                    [ Html.text "Once purged, " ]
                , Html.b
                    []
                    [ Html.text "all" ]
                , Html.span
                    []
                    [ Html.text " memories will be lost " ]
                , Html.b
                    []
                    [ Html.text "forever!" ]
                ]
            , Html.section
                [ Attrs.css
                    [ Css.displayFlex
                    , Css.justifyContent Css.flexEnd
                    ]
                ]
                [ Html.button
                    [ Attrs.css
                        (defaultButtonStyle themeColor)
                    , Events.onClick ExecutePurge
                    ]
                    [ Html.text "Purge" ]
                , Html.button
                    [ Attrs.css
                        (defaultButtonStyle themeColor
                            ++ [ Css.marginLeft <| Css.rem 1 ]
                        )
                    , Events.onClick CancelPurge
                    , Attrs.id "cancelPurgeButton"
                    ]
                    [ Html.text "Keep" ]
                ]
            ]

    else
        Html.text ""


defaultButtonStyle : Float -> List Css.Style
defaultButtonStyle themeColor =
    [ Css.fontSize <| Css.rem 1.2
    , Css.padding2 (Css.rem 0.5) (Css.rem 1)
    , Css.backgroundColor darkColor
    , Css.color <| lightColor themeColor
    , Css.cursor Css.pointer
    ]


backgroundGradient : Float -> Css.Style
backgroundGradient themeColor =
    Css.backgroundImage <| Css.linearGradient (Css.stop darkColor) (Css.stop <| lightColor themeColor) []


viewIdeaStream : Model -> Html Msg
viewIdeaStream { oldThoughts, currentThought, currentZone, themeColor } =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top <| Css.px 0
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , backgroundGradient themeColor
            , Css.fontFamily Css.sansSerif
            , Css.color darkColor
            ]
        ]
        [ Html.h1
            [ Attrs.css
                [ Css.color <| lightColor themeColor ]
            ]
            [ Html.text "Let the ideas flow" ]
        , viewThoughts oldThoughts currentZone
        , viewCurrentThought themeColor currentThought
        ]


viewThoughts : List ( Thought, Posix ) -> Maybe Zone -> Html Msg
viewThoughts oldThoughts maybeZone =
    KeyedHtml.ul
        [ Attrs.css
            [ Css.displayFlex
            , Css.flexDirection Css.columnReverse
            , Css.overflowY Css.auto
            , Css.width <| Css.pct 100
            , Css.listStyle Css.none
            , Css.padding <| Css.px 0
            , Css.alignItems Css.center
            , Css.flex <| Css.int 1
            ]
        ]
    <|
        List.map (viewOldThought maybeZone) oldThoughts


viewOldThought : Maybe Zone -> ( Thought, Posix ) -> ( String, Html msg )
viewOldThought maybeZone ( thoughts, createdAt ) =
    ( createdAt |> Time.posixToMillis |> String.fromInt
    , Html.li
        [ Attrs.css
            [ Css.width <| Css.rem 30
            , Css.lastChild
                [ Css.padding2 (Css.rem 1) (Css.rem 0)
                , Css.paddingTop <| Css.rem 10
                ]
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.border3 (Css.px 1) Css.solid darkColor
                , Css.padding <| Css.rem 1
                ]
            ]
            [ Html.text <|
                case maybeZone of
                    Nothing ->
                        ""

                    Just zone ->
                        let
                            year =
                                Time.toYear zone createdAt

                            month =
                                Time.toMonth zone createdAt

                            day =
                                Time.toDay zone createdAt

                            hour =
                                Time.toHour zone createdAt

                            minute =
                                Time.toMinute zone createdAt
                        in
                        String.fromInt year
                            ++ " "
                            ++ stringFromMonth month
                            ++ " "
                            ++ String.fromInt day
                            ++ ", "
                            ++ String.fromInt hour
                            ++ ":"
                            ++ String.fromInt minute
            , Html.div
                [ Attrs.css
                    [ Css.borderBottom3 (Css.px 1) Css.solid darkColor
                    , Css.height <| Css.px 1
                    , Css.width <| Css.pct 100
                    ]
                ]
                []
            , Html.p
                [ Attrs.css
                    [ Css.whiteSpace Css.preWrap
                    ]
                ]
                [ Html.text thoughts ]
            ]
        ]
    )


stringFromMonth : Month -> String
stringFromMonth month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Jan"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


viewCurrentThought : Float -> Maybe ( Thought, Posix ) -> Html Msg
viewCurrentThought themeColor maybeThought =
    Html.section
        [ Attrs.css
            [ Css.height <| Css.rem 10
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width <| Css.rem 30
            , Css.marginBottom <| Css.rem 1
            ]
        ]
        [ case maybeThought of
            Nothing ->
                Html.button
                    [ Events.onClick CreateThought
                    , Attrs.id "newThoughtButton"
                    , Attrs.css <| defaultButtonStyle themeColor
                    ]
                    [ Html.text "New Thought" ]

            Just ( thought, _ ) ->
                Html.textarea
                    [ Attrs.value thought
                    , Attrs.id "thoughtBox"
                    , Events.onInput UpdateThought
                    , Attrs.css
                        [ Css.height <| Css.calc (Css.pct 100) Css.minus (Css.rem 2)
                        , Css.width <| Css.pct 100
                        , Css.resize Css.none
                        , Css.fontSize <| Css.rem 1
                        , Css.padding <| Css.rem 1
                        ]
                    , Attrs.rows (String.indices thought "\n" |> List.length |> (+) 1)
                    ]
                    []
        ]
