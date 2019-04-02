port module Main exposing (main)

import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrame, onKeyDown, onResize)
import Css
import Css.Transitions as Transitions
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
    , currentThought : ( Thought, Posix )
    , attemptPurge : Bool
    , showAbout : Bool
    , currentZone : Maybe Zone
    , screenSize : ScreenSize
    , isVertical : Bool
    , finding : ( Bool, String )
    , currentTime : Posix
    }


type alias Thought =
    String


type alias Flags =
    { priorThoughts : List ( Thought, Posix )
    , now : Posix
    , width : Int
    , height : Int
    }


type ScreenSize
    = Small
    | Medium
    | Large


type Msg
    = NoOp
    | StartThought Posix
    | UpdateThought String
    | UpdateThoughtTime Posix
    | StoreThoughtAfterTime Posix
    | StoreAndCreateThought
    | AttemptPurge
    | CancelPurge
    | ExecutePurge
    | ShowAbout
    | HideAbout
    | SetZone Zone
    | UpdateSizeAndOrientation Int Int
    | ShowFind
    | HideFind
    | NewSearch String
    | TimeTick Posix



---- INIT ----


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        { priorThoughts, now, width, height } =
            flagsDecoder flags
    in
    ( { oldThoughts =
            case priorThoughts of
                [] ->
                    [ ( "Welcome to Idea Stream", now ) ]

                _ ->
                    priorThoughts
      , currentThought = ( "", now )
      , attemptPurge = False
      , showAbout = False
      , currentZone = Nothing
      , screenSize = calculateScreenSize width
      , isVertical = height > width
      , finding = ( False, "" )
      , currentTime = now
      }
    , Task.perform SetZone Time.here
    )


calculateScreenSize : Int -> ScreenSize
calculateScreenSize width =
    if width > 1200 then
        Large

    else if width > 900 then
        Medium

    else
        Small


flagsDecoder : Value -> Flags
flagsDecoder val =
    case Decode.decodeValue decodeFlags val of
        Ok flags ->
            flags

        Err _ ->
            { priorThoughts = [], now = Time.millisToPosix 0, width = 0, height = 0 }


decodeFlags : Decoder Flags
decodeFlags =
    Decode.map4
        (\maybeThoughts now width height ->
            { priorThoughts = Maybe.withDefault [] maybeThoughts
            , now = Time.millisToPosix now
            , width = width
            , height = height
            }
        )
        (Decode.maybe (Decode.field "priorThoughts" (Decode.list decodeThought)))
        (Decode.field "now" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)


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
        [ Time.every (15 * 1000) StoreThoughtAfterTime
        , onKeyDown decodeKeyDown
        , onAnimationFrame TimeTick
        , onResize UpdateSizeAndOrientation
        ]


decodeKeyDown : Decoder Msg
decodeKeyDown =
    Decode.map3
        keyDownToMsg
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "repeat" Decode.bool)


keyDownToMsg : String -> Bool -> Bool -> Msg
keyDownToMsg key ctrlDown isRepeat =
    if isRepeat then
        NoOp

    else
        case ( key, ctrlDown ) of
            ( "Enter", True ) ->
                StoreAndCreateThought

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

        TimeTick time ->
            ( { model | currentTime = time }, Cmd.none )

        ShowFind ->
            ( { model | finding = ( True, "" ) }
            , focusElement "searchInput"
            )

        HideFind ->
            ( { model | finding = ( False, "" ) }, focusElement "thoughtBox" )

        NewSearch term ->
            ( { model | finding = ( True, term ) }, Cmd.none )

        UpdateSizeAndOrientation width height ->
            ( { model | screenSize = calculateScreenSize width, isVertical = height > width }
            , Cmd.none
            )

        SetZone zone ->
            ( { model | currentZone = Just zone }, Cmd.none )

        ShowAbout ->
            ( { model | showAbout = True }
            , focusElement "hideAboutButton"
            )

        HideAbout ->
            ( { model | showAbout = False }, Cmd.none )

        AttemptPurge ->
            ( { model | attemptPurge = True }
            , focusElement "cancelPurgeButton"
            )

        CancelPurge ->
            ( { model | attemptPurge = False }, Cmd.none )

        ExecutePurge ->
            ( { model | attemptPurge = False, oldThoughts = [], currentThought = ( "", Time.millisToPosix 0 ) }
            , purgeThoughts ()
            )

        StartThought time ->
            ( { model | currentThought = ( "", time ) }
            , focusElement "thoughtBox"
            )

        UpdateThought newContent ->
            let
                ( _, time ) =
                    model.currentThought
            in
            ( { model | currentThought = ( newContent, time ) }
            , Task.perform UpdateThoughtTime Time.now
            )

        UpdateThoughtTime time ->
            let
                ( thought, _ ) =
                    model.currentThought
            in
            ( { model | currentThought = ( thought, time ) }
            , Cmd.none
            )

        StoreThoughtAfterTime now ->
            let
                ( _, time ) =
                    model.currentThought
            in
            if Time.posixToMillis now - Time.posixToMillis time > (5 * 60 * 1000) then
                let
                    nextModel =
                        storeThought model
                in
                ( nextModel
                , Cmd.batch
                    [ writeThoughtsToDisk nextModel.oldThoughts
                    , scrollToCurrentThought
                    ]
                )

            else
                ( model, Cmd.none )

        StoreAndCreateThought ->
            let
                nextModel =
                    storeThought model
            in
            ( nextModel
            , Cmd.batch
                [ writeThoughtsToDisk nextModel.oldThoughts
                , scrollToCurrentThought
                ]
            )


startThought : Cmd Msg
startThought =
    Task.perform StartThought Time.now


scrollToCurrentThought : Cmd Msg
scrollToCurrentThought =
    Dom.getViewportOf "thoughtList"
        |> Task.andThen (\info -> Dom.setViewportOf "thoughtList" 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


focusElement : String -> Cmd Msg
focusElement id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


storeThought : Model -> Model
storeThought ({ currentThought, currentTime } as model) =
    let
        ( thought, t ) =
            currentThought

        trimmedThought =
            String.trim thought

        newThought =
            ( "", currentTime )
    in
    if String.length trimmedThought < 1 then
        { model | currentThought = newThought }

    else
        { model
            | currentThought = newThought
            , oldThoughts = ( trimmedThought, t ) :: model.oldThoughts
        }



---- VIEW ----


secondaryColor : Css.Color
secondaryColor =
    Css.rgb 255 255 255


primaryColor : Css.Color
primaryColor =
    Css.rgb 255 200 200


darkColor : Css.Color
darkColor =
    Css.rgb 45 45 45


view : Model -> Document Msg
view ({ screenSize, isVertical } as model) =
    { title = "Note to Self"
    , body =
        [ Html.toUnstyled <|
            case screenSize of
                Small ->
                    if isVertical then
                        viewApp model

                    else
                        Html.div
                            []
                            [ Html.text "Please rotate vertically" ]

                Medium ->
                    viewApp model

                Large ->
                    viewApp model
        ]
    }


viewApp : Model -> Html Msg
viewApp ({ screenSize } as model) =
    Html.div
        [ Attrs.css
            [ Css.fontSize <|
                Css.rem <|
                    case screenSize of
                        Small ->
                            3

                        Medium ->
                            2

                        Large ->
                            1
            , Css.position Css.absolute
            , Css.top <| Css.rem 0
            , Css.bottom <| Css.rem 0
            , Css.left <| Css.rem 0
            , Css.right <| Css.rem 0
            , backgroundGradient
            , Css.fontFamily Css.sansSerif
            , Css.color darkColor
            ]
        ]
        [ Html.header
            [ Attrs.css
                [ Css.width <| Css.pct 100
                , Css.textAlign Css.center
                , Css.position Css.fixed
                , Css.zIndex <| Css.int 1
                , Css.backgroundImage <| Css.linearGradient (Css.stop2 secondaryColor <| Css.rem 2) (Css.stop <| Css.rgba 255 255 255 0) []
                ]
            ]
            [ Html.h1
                [ Attrs.css
                    [ Css.color primaryColor
                    ]
                ]
                [ Html.text "Note to Self" ]
            ]
        , Html.main_
            [ Attrs.css
                [ Css.position Css.fixed
                , Css.top <| Css.rem 0
                , Css.bottom <| Css.rem 0
                , Css.left <| Css.rem 0
                , Css.right <| Css.rem 0
                ]
            ]
            [ viewThoughts model ]
        , Html.footer
            [ Attrs.css
                [ Css.position Css.fixed
                , Css.bottom <| Css.rem 0
                , Css.width <| Css.pct 100
                , Css.displayFlex
                , Css.justifyContent Css.center
                ]
            ]
            [ viewCurrentThought model ]
        , viewPurgeAttempt model
        , viewAbout model
        ]



-- [ Html.button
--     [ Attrs.css
--         (defaultButtonStyle screenSize
--             ++ [ Css.position Css.absolute
--                , Css.top <| Css.rem 1
--                , Css.right <| Css.rem 1
--                ]
--         )
--     , Events.onClick AttemptPurge
--     ]
--     [ Html.text "Purge?" ]
-- , Html.button
--     [ Attrs.css
--         (defaultButtonStyle screenSize
--             ++ [ Css.position Css.absolute
--                , Css.top <| Css.rem 1
--                , Css.left <| Css.rem 1
--                ]
--         )
--     , Events.onClick ShowAbout
--     ]
--     [ Html.text "About" ]
-- ]


viewModal : ScreenSize -> List (Html Msg) -> Html Msg
viewModal screenSize children =
    Html.div
        [ Attrs.css
            [ Css.position Css.absolute
            , Css.top <| Css.px 0
            , Css.bottom <| Css.px 0
            , Css.left <| Css.px 0
            , Css.right <| Css.px 0
            , Css.zIndex <| Css.int 10
            , Css.backgroundColor <| Css.rgba 0 0 0 0.5
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.fontFamily Css.sansSerif
            , Css.fontSize <|
                Css.rem <|
                    case screenSize of
                        Large ->
                            1.2

                        Medium ->
                            2.2

                        Small ->
                            3.2
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.backgroundColor primaryColor
                , Css.padding <| Css.rem 1
                , Css.maxWidth <| Css.rem 40
                ]
            ]
            children
        ]


viewAbout : Model -> Html Msg
viewAbout { showAbout, screenSize } =
    if showAbout then
        viewModal
            screenSize
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
                        (defaultButtonStyle screenSize)
                    , Events.onClick HideAbout
                    , Attrs.id "hideAboutButton"
                    ]
                    [ Html.text "Back to Your Ideas" ]
                ]
            ]

    else
        emptyHtml


viewPurgeAttempt : Model -> Html Msg
viewPurgeAttempt { attemptPurge, screenSize } =
    if attemptPurge then
        viewModal
            screenSize
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
                        (defaultButtonStyle screenSize)
                    , Events.onClick ExecutePurge
                    ]
                    [ Html.text "Purge" ]
                , Html.button
                    [ Attrs.css
                        (defaultButtonStyle screenSize
                            ++ [ Css.marginLeft <| Css.rem 1 ]
                        )
                    , Events.onClick CancelPurge
                    , Attrs.id "cancelPurgeButton"
                    ]
                    [ Html.text "Keep" ]
                ]
            ]

    else
        emptyHtml


defaultButtonStyle : ScreenSize -> List Css.Style
defaultButtonStyle screenSize =
    [ Css.fontSize <|
        Css.rem <|
            case screenSize of
                Large ->
                    1.2

                _ ->
                    4
    , case screenSize of
        Large ->
            Css.padding2 (Css.rem 0.5) (Css.rem 1)

        _ ->
            Css.padding2 (Css.rem 2) (Css.rem 4)
    , Css.backgroundColor secondaryColor
    , Css.color primaryColor
    , Css.cursor Css.pointer
    , Css.border3 (Css.px 1) Css.solid primaryColor
    ]


backgroundGradient : Css.Style
backgroundGradient =
    Css.backgroundImage <| Css.linearGradient (Css.stop secondaryColor) (Css.stop primaryColor) []


viewThoughts : Model -> Html Msg
viewThoughts ({ oldThoughts } as model) =
    KeyedHtml.ul
        [ Attrs.css
            [ Css.displayFlex
            , Css.flexDirection Css.columnReverse
            , Css.width <| Css.pct 100
            , Css.listStyle Css.none
            , Css.padding <| Css.px 0
            , Css.alignItems Css.center
            , Css.flex <| Css.int 1
            , Css.margin <| Css.rem 0
            , Css.height <| Css.pct 100
            , Css.overflowY Css.auto
            ]
        , Attrs.id "thoughtList"
        ]
        (oldThoughts
            |> List.filterMap (viewOldThought model)
        )


viewOldThought : Model -> ( Thought, Posix ) -> Maybe ( String, Html msg )
viewOldThought { currentZone, screenSize, finding } ( thought, createdAt ) =
    let
        ( isSearching, term ) =
            finding

        lowerTerm =
            String.toLower term

        lowerThought =
            String.toLower thought
    in
    if not isSearching || String.contains lowerTerm lowerThought then
        Just
            ( createdAt |> Time.posixToMillis |> String.fromInt
            , Html.li
                [ Attrs.css
                    [ case screenSize of
                        Large ->
                            Css.width <| Css.rem 40

                        _ ->
                            Css.width <| Css.pct 95
                    , Css.marginTop <| Css.rem 1
                    , Css.lastChild
                        [ Css.paddingTop <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        40

                                    Medium ->
                                        15

                                    Small ->
                                        20
                        ]
                    , Css.firstChild
                        [ Css.paddingBottom <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        10

                                    Medium ->
                                        10

                                    Small ->
                                        10
                        ]
                    ]
                ]
                [ Html.div
                    [ Attrs.css
                        [ Css.border3 (Css.px 1) Css.solid secondaryColor
                        , Css.padding <| Css.rem 1
                        ]
                    ]
                    [ Html.text <|
                        case currentZone of
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
                                    ++ (String.padLeft 2 '0' <| String.fromInt minute)
                    , Html.div
                        [ Attrs.css
                            [ Css.borderBottom3 (Css.px 1) Css.solid secondaryColor
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
                        [ Html.text thought ]
                    ]
                ]
            )

    else
        Nothing


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


viewCurrentThought : Model -> Html Msg
viewCurrentThought { screenSize, currentThought, finding } =
    let
        ( showSearch, searchTerms ) =
            finding

        ( thought, _ ) =
            currentThought
    in
    Html.section
        [ Attrs.css
            [ Css.height <| Css.rem 10
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , case screenSize of
                Large ->
                    Css.width <| Css.rem 40

                _ ->
                    Css.width <| Css.pct 100
            ]
        ]
        [ Html.section
            [ Attrs.css
                [ Css.height <| Css.rem 9
                , Css.displayFlex
                , Css.alignItems Css.center
                ]
            ]
            [ if showSearch then
                Html.input
                    [ Attrs.id "searchInput"
                    , Attrs.type_ "text"
                    , Events.onInput NewSearch
                    , Attrs.value searchTerms
                    , Attrs.css
                        [ Css.fontSize <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        1

                                    Medium ->
                                        2

                                    Small ->
                                        3
                        , Css.width <| Css.rem 30
                        , Css.padding <| Css.rem 1
                        ]
                    ]
                    []

              else
                Html.textarea
                    [ Attrs.value thought
                    , Attrs.id "thoughtBox"
                    , Attrs.autofocus True
                    , Events.onInput UpdateThought
                    , Attrs.css
                        [ Css.height <| Css.rem 5
                        , case screenSize of
                            Large ->
                                Css.width <| Css.rem 30

                            _ ->
                                Css.width <| Css.rem 40
                        , Css.resize Css.none
                        , Css.fontFamily Css.sansSerif
                        , Css.fontSize <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        1

                                    Medium ->
                                        2

                                    Small ->
                                        3
                        , Css.padding <| Css.rem 1
                        ]
                    , Attrs.rows (String.indices thought "\n" |> List.length |> (+) 1)
                    ]
                    []
            ]
        , Html.section
            [ Attrs.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.alignItems Css.flexEnd
                , Css.justifyContent Css.spaceAround
                , Css.property "justify-content" "space-evenly"
                , Css.width <| Css.rem 10
                , Css.height <| Css.rem 10
                ]
            ]
            [ if showSearch then
                emptyHtml

              else
                Html.button
                    [ Events.onClick StoreAndCreateThought
                    , Attrs.id "storeThoughtButton"
                    , Attrs.css <|
                        defaultButtonStyle screenSize
                    , Attrs.disabled <| String.isEmpty thought
                    ]
                    [ Html.text "Save" ]
            , Html.button
                [ Events.onClick <|
                    if showSearch then
                        HideFind

                    else
                        ShowFind
                , Attrs.css <|
                    defaultButtonStyle screenSize
                ]
                [ Html.text <|
                    if showSearch then
                        "Think"

                    else
                        "Search"
                ]
            ]
        ]


emptyHtml : Html msg
emptyHtml =
    Html.text ""
