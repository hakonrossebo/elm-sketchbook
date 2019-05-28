module Sketches.Example3 exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onMouseMove, onResize, onVisibilityChange)
import Color exposing (..)
import Html exposing (Attribute, Html, div, h1, input, span, text)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode
import Random exposing (..)
import Shared exposing (..)
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Task


type alias Bounds =
    { minX : Float
    , minY : Float
    , maxX : Float
    , maxY : Float
    , minDepth : Float
    , maxDepth : Float
    }


bounds : Bounds
bounds =
    { minX = -45
    , minY = -35
    , maxX = 45
    , maxY = 35
    , minDepth = 1
    , maxDepth = 32
    }


type alias Star =
    { x : Float
    , y : Float
    , z : Float
    , px : Int
    , py : Int
    , color : Color
    }


defaultStar : Star
defaultStar =
    { x = 0, y = 0, z = bounds.maxDepth, px = 0, py = 0, color = Color.white }


type alias Model =
    SharedModel
        { mousePosition : Position
        , sketchDrawingArea : Maybe Dom.Element
        , error : Maybe String
        , stars : List Star
        , seed : Seed
        , zVelocity : Float
        , visibility : Visibility
        , fps : Int
        }


type alias Position =
    { x : Float
    , y : Float
    }


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type Msg
    = NoOp
    | OnAnimationFrameDelta Float
    | OnMouseMove Float Float
    | OnSketchDrawingAreaFound Dom.Element
    | OnWindowResize Int Int
    | OnError String
    | OnKeyChange Direction
    | OnVisibilityChange Visibility


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "3D Startrail"
            , markdown = """
Shows a 3D Startrail.
            """
            }

        ( newStars, newSeed ) =
            addStars True (initialSeed 1234) []
    in
    ( { info = info
      , mousePosition = { x = 0, y = 0 }
      , sketchDrawingArea = Nothing
      , error = Nothing
      , stars = newStars
      , seed = newSeed
      , zVelocity = 10
      , visibility = Visible
      , fps = 0
      }
    , getSketchDrawingArea
    )


getSketchDrawingArea : Cmd Msg
getSketchDrawingArea =
    let
        processElement e =
            case e of
                Ok result ->
                    OnSketchDrawingAreaFound result

                Err error ->
                    case error of
                        Dom.NotFound errorInfo ->
                            OnError errorInfo
    in
    Dom.getElement "sketch-drawing-area"
        |> Task.attempt processElement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnAnimationFrameDelta diff ->
            let
                ( newModel, newCmd ) =
                    updateStep model diff

                newFps =
                    calculateAverageFps model.fps (1000.0 / diff)
            in
            ( { newModel | fps = newFps }, newCmd )

        OnMouseMove x y ->
            ( { model | mousePosition = { x = x, y = y } }, Cmd.none )

        OnSketchDrawingAreaFound element ->
            ( { model | sketchDrawingArea = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( model, Cmd.none )

        OnVisibilityChange visibility ->
            let
                zVelocity =
                    case visibility of
                        Visible ->
                            10

                        Hidden ->
                            0
            in
            ( { model | visibility = visibility, zVelocity = zVelocity }, Cmd.none )


newStar : ( Float, Float ) -> Float -> Star
newStar ( newX, newY ) newZ =
    { defaultStar | x = newX, y = newY, z = newZ }


perspective : Float
perspective =
    256


starCount : Int
starCount =
    500


addStars : Bool -> Seed -> List Star -> ( List Star, Seed )
addStars initAllStars seed stars =
    if (starCount - List.length stars) <= 0 then
        ( stars, seed )

    else
        let
            ( star, newSeed ) =
                generateStar initAllStars seed
        in
        addStars initAllStars newSeed (star :: stars)


generateStar : Bool -> Seed -> ( Star, Seed )
generateStar initAllStars seed =
    case initAllStars of
        True ->
            let
                newz =
                    Random.float bounds.minDepth bounds.maxDepth

                ( randomz, newSeed ) =
                    Random.step newz seed

                pair =
                    Random.pair (Random.float bounds.minX bounds.maxX) (Random.float bounds.minY bounds.maxY)

                ( coords, newSeed2 ) =
                    Random.step pair newSeed
            in
            ( newStar coords randomz, newSeed2 )

        False ->
            let
                pair =
                    Random.pair (Random.float bounds.minX bounds.maxX) (Random.float bounds.minY bounds.maxY)

                ( coords, newSeed2 ) =
                    Random.step pair seed
            in
            ( newStar coords bounds.maxDepth, newSeed2 )


updateStep : Model -> Float -> ( Model, Cmd Msg )
updateStep model dt =
    case model.visibility of
        Visible ->
            let
                maxDt =
                    if dt > 30 then
                        30

                    else
                        dt

                ( updatedStars, updatedSeed ) =
                    model.stars
                        |> List.map (moveStar model.zVelocity maxDt)
                        |> List.filter filterVisibleStars
                        |> addStars False model.seed

                newModel =
                    { model
                        | stars = updatedStars
                        , seed = updatedSeed
                    }
            in
            ( newModel
            , Cmd.none
            )

        Hidden ->
            ( model, Cmd.none )


moveStar : Float -> Float -> Star -> Star
moveStar velocity dt star =
    let
        zVelocity =
            (dt * velocity) / 1000
    in
    { star
        | z = star.z - zVelocity
    }


filterVisibleStars : Star -> Bool
filterVisibleStars star =
    abs star.x < bounds.maxX && abs star.y < bounds.maxY && star.z >= bounds.minDepth && star.z <= bounds.maxDepth


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        offsetXDecoder : Decode.Decoder Float
        offsetXDecoder =
            Decode.field "clientX" Decode.float

        offsetYDecoder : Decode.Decoder Float
        offsetYDecoder =
            Decode.field "clientY" Decode.float
    in
    Sub.batch
        [ onAnimationFrameDelta OnAnimationFrameDelta
        , onMouseMove (Decode.map2 OnMouseMove offsetXDecoder offsetYDecoder)
        , onResize OnWindowResize
        , onKeyDown (Decode.map OnKeyChange keyDecoder)
        , onVisibilityChange OnVisibilityChange
        ]


view : Model -> Html Msg
view model =
    case model.error of
        Nothing ->
            div [ class "sketch-default-container" ]
                [ div [ class "sketch-default-top-item" ] [ text "3D Startrail. Random generation of stars and updating depth z value with 2D projection." ]
                , viewSketchDrawingContent model
                , viewMousePositionInformation model
                ]

        Just error ->
            div [ class "sketch-default-container" ]
                [ viewError error
                ]


viewError : String -> Html Msg
viewError error =
    div []
        [ h1 [] [ text error ]
        ]


viewSketchDrawingContent : Model -> Html Msg
viewSketchDrawingContent model =
    div [ class "sketch-default-main-item", id "sketch-drawing-area" ]
        (case model.sketchDrawingArea of
            Just element ->
                let
                    windowWidth =
                        element.element.width
                            |> round
                            |> String.fromInt

                    windowHeight =
                        element.element.height
                            |> round
                            |> String.fromInt

                    viewBoxInfo =
                        "0 0 " ++ windowWidth ++ " " ++ windowHeight
                in
                [ svg
                    [ width windowWidth
                    , height windowHeight
                    , viewBox viewBoxInfo
                    ]
                    [ g [] (drawStars model element.element.width element.element.height)
                    ]
                ]

            Nothing ->
                [ text "Drawing area not ready"
                ]
        )


drawStars : Model -> Float -> Float -> List (Svg b)
drawStars model width height =
    let
        windowHeight =
            String.fromFloat height

        windowWidth =
            String.fromFloat width

        windowCenterHeight =
            height / 2

        windowCenterWidth =
            width / 2
    in
    model.stars
        |> List.sortBy (\star -> star.z)
        |> List.reverse
        |> List.map calculate2DPoint
        |> List.map (drawStar windowCenterWidth windowCenterHeight)


drawStar : Float -> Float -> ( Float, Float, Float ) -> Svg.Svg g
drawStar centerX centerY ( x, y, z ) =
    let
        x_ =
            String.fromFloat (x + centerX)

        y_ =
            String.fromFloat (y + centerY)

        size =
            (1.2 - z / bounds.maxDepth) * 4

        shade =
            1 - z / bounds.maxDepth

        shadeColor =
            rgb shade shade shade
    in
    circle [ cx x_, cy y_, r (String.fromFloat size), fill (Color.toCssString shadeColor) ] []


calculate2DPoint : Star -> ( Float, Float, Float )
calculate2DPoint { x, y, z } =
    let
        k =
            perspective / z

        newX =
            x * k

        newY =
            y * k
    in
    ( newX, newY, z )


viewMousePositionInformation : Model -> Html Msg
viewMousePositionInformation model =
    case model.sketchDrawingArea of
        Just element ->
            let
                mouseX =
                    model.mousePosition.x
                        |> round
                        |> String.fromInt
                        |> (\n -> "Mouse X: " ++ n)

                mouseY =
                    model.mousePosition.y
                        |> round
                        |> String.fromInt
                        |> (\n -> "Mouse Y: " ++ n)

                windowWidth =
                    element.element.width
                        |> round
                        |> String.fromInt
                        |> (\n -> "Window width: " ++ n)

                windowHeight =
                    element.element.height
                        |> round
                        |> String.fromInt
                        |> (\n -> "Window height: " ++ n)

                fps =
                    model.fps
                        |> String.fromInt
                        |> (\n -> "FPS: " ++ n)
            in
            div [ class "sketch-default-footer-item" ]
                [ span [] [ text mouseX ]
                , span [] [ text mouseY ]
                , span [] [ text windowWidth ]
                , span [] [ text windowHeight ]
                , span [] [ text fps ]
                ]

        Nothing ->
            div [ class "sketch-default-footer-item" ]
                [ h1 [] [ text "Sketch drawing area not ready" ]
                ]
