module Game exposing
  (
    Game,
    Block(..),
    Direction,
    Point2D,
    Snake,
    init,
    step,
    at
  )

import Maybe exposing (..)

size = 25

type Direction = Left | Right | Up | Down
type Block = None | Apple | Obstacle | Player Int
type alias Point2D = {x : Int, y : Int}
type alias Snake =
  {
    cells : List Point2D, -- from head to tail
    additionalLength : Int,
    facing: Direction
  }

type alias Game =
  {
    width : Int,
    height : Int,
    snakes : List Snake,
    apples : List Point2D,
    obstacles : List Point2D,
    loser : Maybe Int
  }

init : Game
init =
  {
    width = size,
    height = size,
    snakes = [Snake [Point2D 0 0] 0 Down],
    apples = [],
    obstacles = [],
    loser = Nothing
  }

step : Game -> Game
step = identity

at : Point2D -> Game -> Block
at point game =
  if game.apples |> List.member point then
    Apple
  else if game.obstacles |> List.member point then
    Obstacle
  else if game.snakes |> List.any (\snake -> snake.cells |> List.member point) then
    Player 0
  else
    None
