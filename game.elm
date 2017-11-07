module Game exposing
  (
    Game,
    Block(..),
    Direction(..),
    Point2D,
    Snake,
    init,
    step,
    input,
    finished,
    at
  )

import Maybe exposing (..)
import Tuple exposing (first, second)

size = 25

type Direction = Left | Right | Up | Down
type Block = None | Apple | Obstacle | Player Int
type alias Point2D = {x : Int, y : Int}

type alias Snake =
  {
    head : Point2D,
    cells : List Point2D, -- from head to tail, without head itself
    additionalLength : Int,
    facing: Direction
  }
createSnake : Point2D -> Snake
createSnake cell =
  {
    head = cell,
    cells = [],
    additionalLength = 0,
    facing = Down
  }

updateSnakeFacing : Direction -> Snake -> Snake
updateSnakeFacing newFacing snake =
  {snake | facing = newFacing}
snakeOnCell : Point2D -> Snake -> Bool
snakeOnCell cell snake =
  (snake.head :: snake.cells)
  |> List.member cell

type alias Game =
  {
    width : Int,
    height : Int,
    snakes : List Snake,
    apples : List Point2D,
    obstacles : List Point2D,
    losers : List Int
  }

finished : Game -> Bool
finished game = game.losers /= []

updateGameSnakes : List Snake -> Game -> Game
updateGameSnakes newSnakes game =
  {game | snakes = newSnakes}

init : Game
init =
  {
    width = size,
    height = size,
    snakes = [createSnake (Point2D 0 0)],
    apples = [Point2D 2 2, Point2D 3 3, Point2D 4 4],
    obstacles = [],
    losers = []
  }

stepSnake snake =
  let
    moveForward snake =
      let
        oldHead = snake.head
        newHead =
          case snake.facing of
            Up -> Point2D oldHead.x (oldHead.y - 1)
            Down -> Point2D oldHead.x (oldHead.y + 1)
            Left -> Point2D (oldHead.x - 1) oldHead.y
            Right -> Point2D (oldHead.x + 1) oldHead.y
      in
        {snake | cells = oldHead :: snake.cells, head = newHead}

    cutTail snake =
      if snake.additionalLength == 0 then
        let
          newCells =
            snake.cells
            |> List.reverse
            |> List.drop(1)
            |> List.reverse
        in
          {snake | cells = newCells}
      else
        {snake | additionalLength = snake.additionalLength - 1}
  in
    snake
    |> moveForward
    |> cutTail

step : Game -> Game
step game =
  if game.losers == [] then
    let
      stepSnakes game =
        game |> updateGameSnakes (List.map stepSnake game.snakes)

      -- Enlarge all needed snakes, return new snakes and if the apple is eaten
      eatApple : Point2D -> List Snake -> (Bool, List Snake)
      eatApple apple snakes =
        let
          newSnakes =
            snakes
            |> List.map
                 (\snake ->
                   if snakeOnCell apple snake then
                     (
                       True,
                       {snake | additionalLength = snake.additionalLength + 1}
                     )
                   else
                     (False, snake))
        in
          (List.any first newSnakes, List.map second newSnakes)

      eatApples : Game -> Game
      eatApples game =
        let
          (newSnakes, newApples) =
            List.foldl
              (
                \apple (snakes, apples) ->
                  let
                    (eaten, newSnakes) = eatApple apple snakes
                  in
                    if eaten then
                      (newSnakes, apples)
                    else
                      (snakes, apples ++ [apple])
              )
              (game.snakes, [])
              game.apples
        in
          {game | snakes = newSnakes, apples = newApples}
    in
      -- Move snakes
      game
      |> stepSnakes
      -- Check collision w/ walls and obstacles
      -- Check collision w/ each other
      -- Check collision w/ apples
      |> eatApples
      -- Add apples
  else
    game

input : Int -> Direction -> Game -> Game
input playerNo facing game =
  let
    updateHisSnake i snake =
      if i == playerNo then
        updateSnakeFacing facing snake
      else
        snake
  in
    updateGameSnakes
      (
        List.map2
          updateHisSnake
          (List.range 0 (List.length game.snakes - 1))
          game.snakes
      )
      game

at : Point2D -> Game -> Block
at point game =
  if game.apples |> List.member point then
    Apple
  else if game.obstacles |> List.member point then
    Obstacle
  else if List.any (snakeOnCell point) game.snakes then
    Player 0
  else
    None
