module Main exposing (main)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import Game exposing (Game, Block(..), Point2D)

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

main =
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
