module Main exposing (main)

-- import Keyboard exposing (KeyCode)
-- import Html.Events exposing (onClick)
-- import Browser.Events exposing (onClick)

import Browser
import Collage exposing (Collage, FillStyle, filled, shift, square, uniform)
import Collage.Layout exposing (at, stack, topLeft)
import Collage.Render
import Color
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time


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
        Just a_tail ->
            List.reverse a_tail

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


background : Int -> Collage msg
background a =
    square (toFloat <| unit a + a)
        |> filled (uniform Color.blue)


block : FillStyle -> Position -> Collage msg
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
        |> shift ( toFloat <| new_x, toFloat <| new_y )


main : Program () Model Msg
main =
    Browser.element
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
    , time : Time.Posix
    , collide : Collision
    , gameState : GameState
    }


type GameState
    = Stopped
    | Paused
    | Playing


type Msg
    = NoOp
    | Tick Time.Posix
    | TurnLeft
    | TurnRight
    | Reset
    | NewApple ( Int, Int )


init : () -> ( Model, Cmd Msg )
init flags =
    ( { snake = snakeInit
      , apple = Nothing
      , time = Time.millisToPosix 0
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
          -- Keyboard.downs (key model)
          -- , Window.resizes Resize
          Time.every 400 Tick
        ]



-- key : Model -> KeyCode -> Msg
-- key model keycode =
--     case keycode of
--         37 ->
--             TurnLeft
--
--         39 ->
--             TurnRight
--
--         --        40 ->
--         --            MoveDown
--         --
--         --        38 ->
--         --            MoveUp
--         _ ->
--             NoOp


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
                    case model.gameState of
                        Playing ->
                            let
                                snake =
                                    model.snake

                                movedHead =
                                    moveHead snake.direction snake.head
                            in
                            collisionState movedHead model

                        _ ->
                            model.collide

                movedSnake =
                    if model.gameState == Playing then
                        moveSnake model.snake collideState

                    else
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

                updatedSnake =
                    if gameState == Playing then
                        movedSnake

                    else
                        model.snake
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
            init ()

        NewApple ( x, y ) ->
            ( { model | apple = Just ( x, y ) }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        apple =
            case model.apple of
                Just pos ->
                    [ block (uniform Color.green) pos ]

                Nothing ->
                    []

        snake_head =
            block (uniform Color.white) model.snake.head

        snake_tail =
            List.map (block (uniform Color.yellow)) model.snake.tail

        blocks =
            snake_head :: snake_tail ++ apple ++ [ background boardSize ]
    in
    div []
        [ Collage.Render.svg (stack blocks)
        , button [ style "width" "300px", style "height" "100px", style "font-size" "150%", onClick TurnLeft ] [ text "<" ]
        , button [ style "width" "240px", style "height" "100px", style "font-size" "150%", onClick Reset ] [ text <| String.fromInt <| List.length model.snake.tail ]
        , button [ style "width" "300px", style "height" "100px", style "font-size" "150%", onClick TurnRight ] [ text ">" ]
        ]
