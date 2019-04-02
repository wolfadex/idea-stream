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


appName : String
appName =
    "Notes to Self"



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
    , menuIsOpen : Bool
    , userColor : PrimaryColor
    , showColorPicker : Bool
    }


type alias Thought =
    String


type alias Flags =
    { priorThoughts : List ( Thought, Posix )
    , now : Posix
    , width : Int
    , height : Int
    , colorChoice : PrimaryColor
    }


type ScreenSize
    = Small
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
    | SetMenuOpen Bool
    | SetUserColor PrimaryColor
    | ShowColorPicker
    | HideColorPicker


type PrimaryColor
    = Blue
    | Aqua
    | Teal
    | Olive
    | Green
    | Orange
    | Red
    | Maroon
    | Fuchsia
    | Purple
    | Gray


listOfColors : List PrimaryColor
listOfColors =
    [ Blue
    , Aqua
    , Teal
    , Olive
    , Green
    , Orange
    , Red
    , Maroon
    , Fuchsia
    , Purple
    , Gray
    ]


toCssColor : PrimaryColor -> Css.Color
toCssColor pColor =
    case pColor of
        Blue ->
            Css.hex "#0074D9"

        Aqua ->
            Css.hex "#7FDBFF"

        Teal ->
            Css.hex "#39CCCC"

        Olive ->
            Css.hex "#3D9970"

        Green ->
            Css.hex "#2ECC40"

        Orange ->
            Css.hex "#FF851B"

        Red ->
            Css.hex "#FF4136"

        Maroon ->
            Css.hex "#85144b"

        Fuchsia ->
            Css.hex "#F012BE"

        Purple ->
            Css.hex "#B10DC9"

        Gray ->
            Css.hex "#AAAAAA"


primaryColorToString : PrimaryColor -> String
primaryColorToString pColor =
    case pColor of
        Blue ->
            "Blue"

        Aqua ->
            "Aqua"

        Teal ->
            "Teal"

        Olive ->
            "Olive"

        Green ->
            "Green"

        Orange ->
            "Orange"

        Red ->
            "Red"

        Maroon ->
            "Maroon"

        Fuchsia ->
            "Fuchsia"

        Purple ->
            "Purple"

        Gray ->
            "Gray"



---- INIT ----


init : Value -> ( Model, Cmd Msg )
init flags =
    let
        { priorThoughts, now, width, height, colorChoice } =
            flagsDecoder flags
    in
    ( { oldThoughts =
            case priorThoughts of
                [] ->
                    [ ( "Welcome to \"" ++ appName ++ "\"", now ) ]

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
      , menuIsOpen = False
      , userColor = colorChoice
      , showColorPicker = False
      }
    , Task.perform SetZone Time.here
    )


calculateScreenSize : Int -> ScreenSize
calculateScreenSize width =
    if width > 1200 then
        Large

    else
        Small


flagsDecoder : Value -> Flags
flagsDecoder val =
    case Decode.decodeValue decodeFlags val of
        Ok flags ->
            flags

        Err _ ->
            { priorThoughts = []
            , now = Time.millisToPosix 0
            , width = 0
            , height = 0
            , colorChoice = Green
            }


decodeFlags : Decoder Flags
decodeFlags =
    Decode.map5
        (\maybeThoughts now width height maybeColor ->
            { priorThoughts = Maybe.withDefault [] maybeThoughts
            , now = Time.millisToPosix now
            , width = width
            , height = height
            , colorChoice = Maybe.withDefault Green maybeColor
            }
        )
        (Decode.maybe (Decode.field "priorThoughts" (Decode.list decodeThought)))
        (Decode.field "now" Decode.int)
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.maybe (Decode.field "colorChoice" Decode.string |> Decode.andThen decodeColor))


decodeColor : String -> Decoder PrimaryColor
decodeColor str =
    Decode.succeed <|
        case str of
            "Blue" ->
                Blue

            "Aqua" ->
                Aqua

            "Teal" ->
                Teal

            "Olive" ->
                Olive

            "Green" ->
                Green

            "Orange" ->
                Orange

            "Red" ->
                Red

            "Maroon" ->
                Maroon

            "Fuchsia" ->
                Fuchsia

            "Purple" ->
                Purple

            "Gray" ->
                Gray

            _ ->
                Green


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


port saveColorChoice : String -> Cmd msg



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ShowColorPicker ->
            ( { model
                | showColorPicker = True
                , menuIsOpen =
                    case model.screenSize of
                        Large ->
                            False

                        Small ->
                            True
              }
            , focusElement "hideColorPickerButton"
            )

        HideColorPicker ->
            ( { model | showColorPicker = False }, focusThoughtBox )

        SetUserColor color ->
            ( { model | userColor = color }
            , saveColorChoice <| primaryColorToString color
            )

        SetMenuOpen open ->
            ( { model | menuIsOpen = open }, Cmd.none )

        TimeTick time ->
            ( { model | currentTime = time }, Cmd.none )

        ShowFind ->
            ( { model
                | finding = ( True, "" )
                , menuIsOpen =
                    case model.screenSize of
                        Large ->
                            model.menuIsOpen

                        Small ->
                            False
              }
            , focusElement "searchInput"
            )

        HideFind ->
            ( { model | finding = ( False, "" ) }, focusThoughtBox )

        NewSearch term ->
            ( { model | finding = ( True, term ) }, Cmd.none )

        UpdateSizeAndOrientation width height ->
            ( { model | screenSize = calculateScreenSize width, isVertical = height > width }
            , Cmd.none
            )

        SetZone zone ->
            ( { model | currentZone = Just zone }, Cmd.none )

        ShowAbout ->
            ( { model
                | showAbout = True
                , menuIsOpen =
                    case model.screenSize of
                        Large ->
                            False

                        Small ->
                            True
              }
            , focusElement "hideAboutButton"
            )

        HideAbout ->
            ( { model | showAbout = False }, focusThoughtBox )

        AttemptPurge ->
            ( { model
                | attemptPurge = True
                , menuIsOpen =
                    case model.screenSize of
                        Large ->
                            False

                        Small ->
                            True
              }
            , focusElement "cancelPurgeButton"
            )

        CancelPurge ->
            ( { model | attemptPurge = False }, focusThoughtBox )

        ExecutePurge ->
            ( { model | attemptPurge = False, oldThoughts = [], currentThought = ( "", Time.millisToPosix 0 ) }
            , Cmd.batch
                [ purgeThoughts ()
                , focusThoughtBox
                ]
            )

        StartThought time ->
            ( { model | currentThought = ( "", time ) }
            , focusThoughtBox
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
                , focusThoughtBox
                ]
            )


focusThoughtBox : Cmd Msg
focusThoughtBox =
    focusElement "thoughtBox"


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



-- primaryColor : Css.Color
-- primaryColor =
--     Css.rgb 255 200 200


darkColor : Css.Color
darkColor =
    Css.rgb 45 45 45


view : Model -> Document Msg
view ({ screenSize, isVertical } as model) =
    { title = appName
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

                Large ->
                    viewApp model
        ]
    }


viewApp : Model -> Html Msg
viewApp ({ screenSize, userColor } as model) =
    Html.div
        [ Attrs.css
            [ Css.fontSize <|
                Css.rem <|
                    case screenSize of
                        Small ->
                            2

                        Large ->
                            1
            , Css.position Css.absolute
            , Css.top <| Css.rem 0
            , Css.bottom <| Css.rem 0
            , Css.left <| Css.rem 0
            , Css.right <| Css.rem 0
            , backgroundGradient userColor
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
                , Css.backgroundImage <|
                    Css.linearGradient
                        (Css.stop2 secondaryColor <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        2

                                    Small ->
                                        6
                        )
                        (Css.stop <| Css.rgba 255 255 255 0)
                        []
                ]
            ]
            [ Html.h1
                [ Attrs.css
                    [ Css.color <| toCssColor userColor
                    ]
                ]
                [ Html.text appName ]
            , case screenSize of
                Small ->
                    viewSmallMenu model

                Large ->
                    emptyHtml
            ]
        , Html.main_
            [ Attrs.css
                [ Css.position Css.fixed
                , Css.top <| Css.rem 0
                , Css.bottom <| Css.rem 0
                , Css.left <| Css.rem 0
                , Css.right <| Css.rem 0
                , Css.displayFlex
                ]
            ]
            [ Html.section
                [ Attrs.css
                    [ Css.flex <| Css.int 1
                    , Css.height <| Css.pct 100
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    ]
                ]
                [ viewThoughts model
                , viewCurrentThought model
                ]
            , case screenSize of
                Large ->
                    viewLargeMenu model

                Small ->
                    emptyHtml
            ]
        , viewPurgeAttempt model
        , viewAbout model
        , viewColorPicker model
        ]


viewColorPicker : Model -> Html Msg
viewColorPicker { showColorPicker, screenSize, userColor } =
    if showColorPicker then
        viewModal userColor
            screenSize
            [ Html.span
                []
                [ Html.text "Choose a Color"
                ]
            , Html.ul
                [ Attrs.css
                    [ Css.listStyle Css.none
                    , Css.padding <| Css.rem 0
                    , Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.flexWrap Css.wrap
                    , Css.maxHeight <| Css.rem 60
                    , Css.alignItems Css.center
                    ]
                ]
                (listOfColors
                    |> List.map
                        (\color ->
                            Html.li
                                [ Attrs.css
                                    [ Css.marginTop <| Css.rem 1 ]
                                ]
                                [ Html.button
                                    [ Attrs.css
                                        (buttonStyle userColor screenSize
                                            ++ [ Css.color <| toCssColor color
                                               , Css.backgroundColor secondaryColor
                                               ]
                                        )
                                    , Events.onClick <| SetUserColor color
                                    ]
                                    [ Html.text <| primaryColorToString color ]
                                ]
                        )
                )
            , Html.button
                [ Attrs.css <|
                    primaryButtonStyle userColor screenSize
                , Events.onClick HideColorPicker
                , Attrs.id "hideColorPickerButton"
                ]
                [ Html.text "Keep Selected Color" ]
            ]

    else
        emptyHtml


viewSmallMenu : Model -> Html Msg
viewSmallMenu ({ menuIsOpen, screenSize, userColor } as model) =
    Html.nav
        [ Attrs.css
            [ Css.position Css.fixed
            , Css.top <| Css.rem 0
            , Css.bottom <| Css.rem 0

            -- , Css.displayFlex
            -- , Css.flexDirection Css.columnReverse
            , Css.backgroundColor secondaryColor
            , Css.width <|
                Css.pct <|
                    if menuIsOpen then
                        100

                    else
                        0
            ]
        ]
        [ viewSmallMmenuButton model
        , Html.ul
            [ Attrs.css
                [ Css.listStyle Css.none
                , Css.paddingLeft <| Css.rem 0
                , Css.marginTop <| Css.rem 10
                ]
            ]
          <|
            if menuIsOpen then
                [ menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick ShowFind
                        ]
                        [ Html.text "Search" ]
                , menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick ShowColorPicker
                        ]
                        [ Html.text "Set Color" ]
                , menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick ShowAbout
                        ]
                        [ Html.text "About" ]
                , menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick AttemptPurge
                        ]
                        [ Html.text "Purge?" ]
                ]

            else
                []
        ]


viewLargeMenu : Model -> Html Msg
viewLargeMenu ({ menuIsOpen, screenSize, userColor } as model) =
    Html.nav
        [ Attrs.css
            [ Css.height <| Css.pct 100
            , Css.borderLeft3 (Css.px 1) Css.solid darkColor
            , Css.displayFlex
            , Css.flexDirection Css.columnReverse
            , Css.width <|
                Css.rem <|
                    if menuIsOpen then
                        10

                    else
                        0
            ]
        ]
        [ viewLargeMmenuButton model
        , Html.ul
            [ Attrs.css
                [ Css.listStyle Css.none
                , Css.paddingLeft <| Css.rem 1
                ]
            ]
          <|
            if menuIsOpen then
                [ menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick ShowColorPicker
                        ]
                        [ Html.text "Set Color" ]
                , menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick ShowAbout
                        ]
                        [ Html.text "About" ]
                , menuItem <|
                    Html.button
                        [ Attrs.css <|
                            buttonStyle userColor screenSize
                        , Events.onClick AttemptPurge
                        ]
                        [ Html.text "Purge?" ]
                ]

            else
                []
        ]


menuItem : Html Msg -> Html Msg
menuItem child =
    Html.li
        [ Attrs.css
            [ Css.marginTop <| Css.rem 1 ]
        ]
        [ child ]


viewSmallMmenuButton : Model -> Html Msg
viewSmallMmenuButton { menuIsOpen, showAbout, attemptPurge, showColorPicker } =
    Html.button
        [ Attrs.css
            [ Css.position Css.fixed
            , Css.top <| Css.rem 3
            , Css.right <| Css.rem 3
            , Css.border <| Css.px 0
            , Css.backgroundColor <| Css.rgba 0 0 0 0
            , Css.marginLeft <| Css.rem -5
            , Css.cursor Css.pointer
            , Css.width <| Css.rem 4
            , Css.height <| Css.rem 4
            , Css.padding <| Css.rem 0
            ]
        , Events.onClick <| SetMenuOpen <| not menuIsOpen
        , Attrs.disabled <| showAbout || attemptPurge || showColorPicker
        ]
    <|
        if menuIsOpen then
            [ Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 4
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transforms
                        [ Css.translateY <| Css.rem 0.25
                        , Css.rotate <| Css.deg 45
                        ]
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 4
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transforms
                        [ Css.translateY <| Css.rem -0.25
                        , Css.rotate <| Css.deg -45
                        ]
                    ]
                ]
                []
            ]

        else
            [ Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 4
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem -0.5
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 4
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem 0
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 4
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem 0.5
                    ]
                ]
                []
            ]


viewLargeMmenuButton : Model -> Html Msg
viewLargeMmenuButton { menuIsOpen, showAbout, attemptPurge, showColorPicker } =
    Html.button
        [ Attrs.css
            [ Css.border <| Css.px 0
            , Css.backgroundColor <| Css.rgba 0 0 0 0
            , Css.position Css.fixed
            , Css.marginLeft <| Css.rem -5
            , Css.bottom <| Css.rem 1
            , Css.cursor Css.pointer
            , Css.width <| Css.rem 3
            , Css.height <| Css.rem 3
            , Css.padding <| Css.rem 0
            ]
        , Events.onClick <| SetMenuOpen <| not menuIsOpen
        , Attrs.disabled <| showAbout || attemptPurge || showColorPicker
        ]
    <|
        if menuIsOpen then
            [ Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 3
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transforms
                        [ Css.translateY <| Css.rem 0.25
                        , Css.rotate <| Css.deg 45
                        ]
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 3
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transforms
                        [ Css.translateY <| Css.rem -0.25
                        , Css.rotate <| Css.deg -45
                        ]
                    ]
                ]
                []
            ]

        else
            [ Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 3
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem -0.25
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 3
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem 0
                    ]
                ]
                []
            , Html.div
                [ Attrs.css
                    [ Css.width <| Css.rem 3
                    , Css.height <| Css.rem 0.5
                    , Css.borderRadius <| Css.rem 100
                    , Css.backgroundColor <| Css.rgb 0 0 0
                    , Css.transform <|
                        Css.translateY <|
                            Css.rem 0.25
                    ]
                ]
                []
            ]


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


viewModal : PrimaryColor -> ScreenSize -> List (Html Msg) -> Html Msg
viewModal userColor screenSize children =
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
            , Css.overflowY Css.auto
            , Css.fontSize <|
                Css.rem <|
                    case screenSize of
                        Large ->
                            1.2

                        Small ->
                            3.2
            ]
        ]
        [ Html.div
            [ Attrs.css
                [ Css.backgroundColor <|
                    case screenSize of
                        Large ->
                            toCssColor userColor

                        Small ->
                            secondaryColor
                , Css.padding <|
                    Css.rem <|
                        case screenSize of
                            Large ->
                                1

                            Small ->
                                3
                , case screenSize of
                    Large ->
                        Css.maxWidth <| Css.rem 40

                    Small ->
                        Css.maxWidth <| Css.pct 100
                ]
            ]
            children
        ]


viewAbout : Model -> Html Msg
viewAbout { showAbout, screenSize, userColor } =
    if showAbout then
        viewModal userColor
            screenSize
            [ Html.p
                [ Attrs.css
                    [ Css.whiteSpace Css.preWrap ]
                ]
                [ Html.span
                    []
                    [ Html.text """A simple app for storing thoughts.

All thoughts are stored in localStorage (in your browser).
There's a button to purge all imformation, because the data is yours and only yours.

If you'd like to see the code, it can be found at """
                    , Html.a
                        [ Attrs.href "https://github.com/wolfadex/idea-stream"
                        , Attrs.target "_blank"
                        ]
                        [ Html.text "idea-stream on GitHub" ]
                    , Html.text "."
                    ]
                ]
            , Html.section
                [ Attrs.css
                    [ Css.displayFlex
                    , Css.justifyContent Css.flexEnd
                    ]
                ]
                [ Html.button
                    [ Attrs.css <|
                        primaryButtonStyle userColor screenSize
                    , Events.onClick HideAbout
                    , Attrs.id "hideAboutButton"
                    ]
                    [ Html.text "Back to Your Ideas" ]
                ]
            ]

    else
        emptyHtml


viewPurgeAttempt : Model -> Html Msg
viewPurgeAttempt { attemptPurge, screenSize, userColor } =
    if attemptPurge then
        viewModal userColor
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
                        (buttonStyle userColor screenSize)
                    , Events.onClick ExecutePurge
                    ]
                    [ Html.text "Purge" ]
                , Html.button
                    [ Attrs.css
                        (primaryButtonStyle userColor screenSize
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


buttonStyle : PrimaryColor -> ScreenSize -> List Css.Style
buttonStyle userColor screenSize =
    [ Css.fontSize <|
        Css.rem <|
            case screenSize of
                Large ->
                    1.2

                Small ->
                    3
    , case screenSize of
        Large ->
            Css.padding2 (Css.rem 0.5) (Css.rem 1)

        Small ->
            Css.padding2 (Css.rem 2) (Css.rem 4)
    , Css.backgroundColor <|
        case screenSize of
            Large ->
                toCssColor userColor

            Small ->
                secondaryColor
    , Css.color <|
        case screenSize of
            Large ->
                secondaryColor

            Small ->
                toCssColor userColor
    , Css.cursor Css.pointer
    , Css.border3
        (Css.px <|
            case screenSize of
                Large ->
                    1

                Small ->
                    4
        )
        Css.solid
        (case screenSize of
            Large ->
                secondaryColor

            Small ->
                toCssColor userColor
        )
    ]


primaryButtonStyle : PrimaryColor -> ScreenSize -> List Css.Style
primaryButtonStyle userColor screenSize =
    [ Css.fontSize <|
        Css.rem <|
            case screenSize of
                Large ->
                    1.2

                Small ->
                    3
    , case screenSize of
        Large ->
            Css.padding2 (Css.rem 0.5) (Css.rem 1)

        Small ->
            Css.padding2 (Css.rem 2) (Css.rem 4)
    , Css.backgroundColor <|
        case screenSize of
            Large ->
                secondaryColor

            Small ->
                toCssColor userColor
    , Css.color <|
        case screenSize of
            Large ->
                toCssColor userColor

            Small ->
                secondaryColor
    , Css.cursor Css.pointer
    , Css.border3
        (Css.px <|
            case screenSize of
                Large ->
                    1

                Small ->
                    4
        )
        Css.solid
        (case screenSize of
            Large ->
                toCssColor userColor

            Small ->
                secondaryColor
        )
    ]


backgroundGradient : PrimaryColor -> Css.Style
backgroundGradient userColor =
    Css.backgroundImage <| Css.linearGradient (Css.stop secondaryColor) (Css.stop <| toCssColor userColor) []


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

                        Small ->
                            Css.width <| Css.pct 95
                    , Css.marginTop <| Css.rem 1
                    , Css.lastChild
                        [ Css.paddingTop <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        40

                                    Small ->
                                        60
                        ]
                    ]
                ]
                [ Html.div
                    [ Attrs.css
                        [ Css.border3
                            (Css.px <|
                                case screenSize of
                                    Large ->
                                        1

                                    Small ->
                                        4
                            )
                            Css.solid
                            secondaryColor
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
                            [ Css.borderBottom3
                                (Css.px <|
                                    case screenSize of
                                        Large ->
                                            1

                                        Small ->
                                            4
                                )
                                Css.solid
                                secondaryColor
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
viewCurrentThought { screenSize, currentThought, finding, userColor, showAbout, attemptPurge, showColorPicker } =
    let
        ( showSearch, searchTerms ) =
            finding

        ( thought, _ ) =
            currentThought

        modalIsOpen =
            showAbout || attemptPurge || showColorPicker
    in
    Html.section
        [ Attrs.css
            [ Css.height <| Css.rem 10
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.width <| Css.pct 100
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
                    , Attrs.disabled modalIsOpen
                    , Attrs.value searchTerms
                    , Attrs.css
                        [ Css.fontSize <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        1

                                    Small ->
                                        3
                        , Css.width <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        30

                                    Small ->
                                        40
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
                    , Attrs.disabled modalIsOpen
                    , Attrs.css
                        [ Css.height <| Css.rem 5
                        , Css.width <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        30

                                    Small ->
                                        40
                        , Css.resize Css.none
                        , Css.fontFamily Css.sansSerif
                        , Css.fontSize <|
                            Css.rem <|
                                case screenSize of
                                    Large ->
                                        1

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
                , Css.width <|
                    Css.rem <|
                        case screenSize of
                            Large ->
                                10

                            Small ->
                                16
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
                        primaryButtonStyle userColor screenSize
                    , Attrs.disabled <| String.isEmpty thought || modalIsOpen
                    ]
                    [ Html.text "Save" ]
            , case screenSize of
                Large ->
                    Html.button
                        [ Events.onClick <|
                            if showSearch then
                                HideFind

                            else
                                ShowFind
                        , Attrs.css <|
                            buttonStyle userColor screenSize
                        , Attrs.disabled modalIsOpen
                        ]
                        [ Html.text <|
                            if showSearch then
                                "Think"

                            else
                                "Search"
                        ]

                Small ->
                    if showSearch then
                        Html.button
                            [ Events.onClick HideFind
                            , Attrs.css <|
                                buttonStyle userColor screenSize
                            , Attrs.disabled modalIsOpen
                            ]
                            [ Html.text "Think"
                            ]

                    else
                        emptyHtml
            ]
        ]


emptyHtml : Html msg
emptyHtml =
    Html.text ""
