module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Keyboard
import Char
import Time exposing (Time)
import Random exposing (minInt, maxInt)

import Game exposing (Game, Block(..), Direction(..), Point2D)

-- Constants

blockSize = 35
frameSize = 4
centerSize = 18
fgColor blk =
  case blk of
    None -> "#657A6E"
    Apple -> "#a82d43"
    Obstacle -> "#000"
    Player _ -> "#060000"
bgColor blk =
  "#9eaea5"

tickLength = Time.second / 2

centerPosition = (blockSize - centerSize) // 2

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
  g
    []
    [
      rect
        [
          fill (bgColor blk),
          stroke (fgColor blk),
          strokeWidth (toString frameSize),
          Svg.Attributes.x (toString (x*blockSize + frameSize)),
          Svg.Attributes.y (toString (y*blockSize + frameSize)),
          Svg.Attributes.width (toString (blockSize - frameSize)),
          Svg.Attributes.height (toString (blockSize - frameSize))
        ]
        [],
      rect
        [
          fill (fgColor blk),
          stroke "none",
          strokeWidth "0",
          Svg.Attributes.x (toString (x*blockSize + centerPosition + frameSize//2)),
          Svg.Attributes.y (toString (y*blockSize + centerPosition + frameSize//2)),
          Svg.Attributes.width (toString centerSize),
          Svg.Attributes.height (toString centerSize)
        ]
        []
    ]

-- Main

type Event =
  Start Int
  | Keypress Char
  | Tick Time

main =
  Html.program {
    init = (Game.init 1, Random.generate Start (Random.int minInt maxInt)),
    update = update,
    view = view,
    subscriptions = subscriptions
  }

update event game =
  if Game.finished game then
    (Game.init 1, Random.generate Start (Random.int minInt maxInt))
  else
    let
      nextState =
    case event of
      Start seed -> Game.init seed
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
  Html.div
    [
      Svg.Attributes.style "font-size: 0; vertical-align: top"
    ]
    [
      svg
        [
          version "1.1",
          x "0",
          y "0",
          Svg.Attributes.width (toString (blockSize*game.width + frameSize + 1)),
          Svg.Attributes.height (toString (blockSize*game.height + frameSize + 1)),
          viewBox (
            "0 0 " ++
            toString (blockSize*game.width + frameSize + 1) ++
            " " ++
            toString (blockSize*game.height + frameSize + 1)
          )
        ]
        (
          listProduct
            (List.range 0 (game.width - 1))
            (List.range 0 (game.height - 1))
          |> List.map (\(x,y) -> square x y (Game.at (Point2D x y) game))
        )
    ]
