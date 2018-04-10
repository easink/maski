module Main exposing (..)

import Collage exposing (Form)
import Element
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode)
import Time
import Random


boardSize : Int
boardSize =
    40


type Direction
    = Up
    | Down
    | Left
    | Right


type Collision
    = Apple
    | Border
    | Tail
    | Board


unit : Int -> Int
unit x =
    20 * x


type alias Position =
    ( Int, Int )


moveUp : Position -> Position
moveUp ( x, y ) =
    ( x, y + 1 )


moveDown : Position -> Position
moveDown ( x, y ) =
    ( x, y - 1 )


moveRight : Position -> Position
moveRight ( x, y ) =
    ( x + 1, y )


moveLeft : Position -> Position
moveLeft ( x, y ) =
    ( x - 1, y )


turnLeft : Snake -> Snake
turnLeft snake =
    let
        dir =
            case snake.direction of
                Up ->
                    Left

                Down ->
                    Right

                Left ->
                    Down

                Right ->
                    Up
    in
        { snake | direction = dir }


turnRight : Snake -> Snake
turnRight snake =
    let
        dir =
            case snake.direction of
                Up ->
                    Right

                Down ->
                    Left

                Left ->
                    Up

                Right ->
                    Down
    in
        { snake | direction = dir }


moveHead : Direction -> Position -> Position
moveHead dir pos =
    case dir of
        Up ->
            moveUp pos

        Down ->
            moveDown pos

        Left ->
            moveLeft pos

        Right ->
            moveRight pos


removeLastTail : List Position -> List Position
removeLastTail tail =
    case List.tail (List.reverse tail) of
        Just tail ->
            List.reverse tail

        Nothing ->
            []


moveSnake : Snake -> Collision -> Snake
moveSnake snake collide =
    let
        updatedTail =
            case collide of
                Apple ->
                    snake.head :: snake.tail

                Board ->
                    snake.head :: removeLastTail snake.tail

                _ ->
                    snake.tail
    in
        { snake | tail = updatedTail, head = moveHead snake.direction snake.head }


background : Int -> Form
background a =
    Collage.square (toFloat <| (unit a) + a)
        |> Collage.filled (Color.blue)


block : Color.Color -> Position -> Form
block color ( x, y ) =
    let
        origin =
            (unit boardSize + boardSize - unit 1) // 2

        new_x =
            unit x - origin + x

        new_y =
            unit y - origin + y
    in
        Collage.square (toFloat <| unit 1)
            |> Collage.filled color
            |> Collage.move ( toFloat <| new_x, toFloat <| new_y )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Snake =
    { tail : List Position
    , head : Position
    , direction : Direction
    }


type alias Model =
    { snake : Snake
    , apple : Maybe Position
    , time : Time.Time
    , collide : Collision
    , gameState : GameState
    }


type GameState
    = Stopped
    | Paused
    | Playing


type Msg
    = NoOp
    | Tick Time.Time
    | TurnLeft
    | TurnRight
    | Reset
    | NewApple ( Int, Int )


init : ( Model, Cmd Msg )
init =
    ( { snake = snakeInit
      , apple = Nothing
      , time = 0
      , collide = Board
      , gameState = Playing
      }
    , Cmd.none
    )


snakeInit : Snake
snakeInit =
    { tail = [ ( 18, 19 ), ( 17, 19 ) ]
    , head = ( 19, 19 )
    , direction = Up
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Keyboard.ups (key model)
          Keyboard.downs (key model)
          -- , Window.resizes Resize
        , Time.every (100 * Time.millisecond) Tick
        ]


key : Model -> KeyCode -> Msg
key model keycode =
    case keycode of
        37 ->
            TurnLeft

        39 ->
            TurnRight

        --        40 ->
        --            MoveDown
        --
        --        38 ->
        --            MoveUp
        _ ->
            NoOp


collisionState : Position -> Model -> Collision
collisionState head model =
    if Just head == model.apple then
        Apple
    else if List.member head model.snake.tail then
        Tail
    else if not (onBoard head) then
        Border
    else
        Board


onBoard : Position -> Bool
onBoard ( x, y ) =
    if x >= 0 && x < boardSize && y >= 0 && y < boardSize then
        True
    else
        False


generateApple : Maybe Position -> Cmd Msg
generateApple apple =
    case apple of
        Just _ ->
            Cmd.none

        Nothing ->
            Random.generate NewApple (Random.pair (Random.int 0 (boardSize - 1)) (Random.int 0 (boardSize - 1)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        TurnLeft ->
            ( { model | snake = turnLeft model.snake }
            , Cmd.none
            )

        TurnRight ->
            ( { model | snake = turnRight model.snake }
            , Cmd.none
            )

        Tick time ->
            let
                newAppleCmd =
                    generateApple model.apple

                collideState =
                    collisionState model.snake.head model

                updatedSnake =
                    case model.gameState of
                        Playing ->
                            moveSnake model.snake collideState

                        _ ->
                            model.snake

                updatedApple =
                    if collideState == Apple then
                        Nothing
                    else
                        model.apple

                gameState =
                    case collideState of
                        Tail ->
                            Stopped

                        Border ->
                            Stopped

                        _ ->
                            Playing
            in
                ( { model
                    | snake = updatedSnake
                    , apple = updatedApple
                    , time = time
                    , collide = collideState
                    , gameState = gameState
                  }
                , newAppleCmd
                )

        Reset ->
            init

        NewApple ( x, y ) ->
            ( { model | apple = Just ( x, y ) }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        apple =
            case model.apple of
                Just pos ->
                    [ block Color.green pos ]

                Nothing ->
                    []

        blocks =
            background boardSize :: block Color.white model.snake.head :: List.map (block Color.yellow) model.snake.tail ++ apple
    in
        div []
            [ blocks
                |> Collage.collage (unit boardSize + boardSize) (unit boardSize + boardSize)
                |> Element.toHtml
            , button [ style [ ( "width", "300px" ), ( "height", "100px" ), ( "font-size", "150%" ) ], onClick TurnLeft ] [ text "<" ]
            , button [ style [ ( "width", "240px" ), ( "height", "100px" ), ( "font-size", "150%" ) ], onClick Reset ] [ text <| toString <| List.length model.snake.tail ]
            , button [ style [ ( "width", "300px" ), ( "height", "100px" ), ( "font-size", "150%" ) ], onClick TurnRight ] [ text ">" ]
            , div [] [ text (toString model) ]
            ]
