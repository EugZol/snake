module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Keyboard
import Char
import Time exposing (Time)

import Game exposing (Game, Block(..), Direction(..), Point2D)

-- Constants

blockSize = 25
blockColor blk =
  case blk of
    None -> "#93a0f2"
    Apple -> "#cc1414"
    Obstacle -> "#000"
    Player _ -> "#27db13"

screenWidth = blockSize * game.width
screenHeight = blockSize * game.height

tickLength = Time.second / 2

-- Helper functions

listProduct : List a -> List b -> List (a, b)
listProduct xs ys =
  xs
  |> List.concatMap (\x -> List.map (\y -> (x,y)) ys)

pointsListToString : List Point2D -> String
pointsListToString points =
  points
  |> List.map (\{x,y} -> (toString x) ++ "," ++ (toString y))
  |> String.join " "

square : Int -> Int -> Block -> Svg msg
square x y blk =
  polygon
    [
      fill <| blockColor blk,
      stroke "#00f",
      points <|
        pointsListToString
          [
            Point2D (x*blockSize) (y*blockSize),
            Point2D (x*blockSize + blockSize - 1) (y*blockSize),
            Point2D (x*blockSize + blockSize - 1) (y*blockSize + blockSize - 1),
            Point2D (x*blockSize) (y*blockSize + blockSize - 1)
          ]
    ]
    []

-- Main

game = Game.init

type Event =
  Keypress Char
  | Tick Time

main =
  Html.program {
    init = (Game.init, Cmd.none),
    update = update,
    view = view,
    subscriptions = subscriptions
  }

update event game =
  let
    nextState =
      if Game.finished game then
        Game.init
      else
        case event of
          Tick _ -> Game.step game
          Keypress 'a' -> Game.input 0 Left game
          Keypress 'd' -> Game.input 0 Right game
          Keypress 'w' -> Game.input 0 Up game
          Keypress 's' -> Game.input 0 Down game
          Keypress _ -> game
  in
    (nextState, Cmd.none)

subscriptions _ =
  Sub.batch [
    Keyboard.presses (Char.fromCode >> Keypress),
    Time.every tickLength Tick
  ]

view game =
  svg
    [
      version "1.1",
      x "0",
      y "0",
      width (toString screenWidth),
      height (toString screenHeight),
      viewBox ("0 0 " ++ toString screenWidth ++ " " ++ toString screenHeight)
    ]
    (
      listProduct
        (List.range 0 (game.width - 1))
        (List.range 0 (game.height - 1))
      |> List.map (\(x,y) -> square x y (Game.at (Point2D x y) game))
    )
