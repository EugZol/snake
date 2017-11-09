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
import Random exposing (Seed)

size = 25

type Direction = Left | Right | Up | Down
opposite : Direction -> Direction
opposite direction =
  case direction of
    Left -> Right
    Right -> Left
    Up -> Down
    Down -> Up

type Block = None | Apple | Obstacle | Player Int
type alias Point2D = {x : Int, y : Int}

type alias Snake =
  {
    head : Point2D,
    cells : List Point2D, -- from head to tail, without head itself
    additionalLength : Int,
    facing: Direction,
    pendingFacing: Direction
  }
createSnake : Point2D -> Snake
createSnake cell =
  {
    head = cell,
    cells = [],
    additionalLength = 0,
    facing = Down,
    pendingFacing = Down
  }

updateSnakeFacing : Direction -> Snake -> Snake
updateSnakeFacing newFacing snake =
  {snake | facing = newFacing}

updateSnakePendingFacing : Direction -> Snake -> Snake
updateSnakePendingFacing newFacing snake =
  {snake | pendingFacing = newFacing}

snakeOnCell : Point2D -> Snake -> Bool
snakeOnCell cell snake =
  (snake.head :: snake.cells)
  |> List.member cell

snakeOnBoard : Game -> Snake -> Bool
snakeOnBoard game snake =
  List.all
    (\cell -> cell.x >= 0 && cell.y >= 0 && cell.x < game.width && cell.y < game.height)
    (snake.head :: snake.cells)

type alias Game =
  {
    width : Int,
    height : Int,
    snakes : List Snake,
    apples : List Point2D,
    obstacles : List Point2D,
    losers : List Int,
    seed : Seed
  }

finished : Game -> Bool
finished game = game.losers /= []

updateGameSnakes : List Snake -> Game -> Game
updateGameSnakes newSnakes game =
  {game | snakes = newSnakes}

init : Int -> Game
init randomInt =
  {
    width = size,
    height = size,
    snakes = [createSnake (Point2D 0 0)],
    apples = [],
    obstacles = [],
    losers = [],
    seed = Random.initialSeed randomInt
  }

stepSnake snake =
  let
    turn snake =
      if not (opposite snake.facing == snake.pendingFacing) then
        {snake | facing = snake.pendingFacing}
      else
        {snake | pendingFacing = snake.facing}

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
    |> turn
    |> moveForward
    |> cutTail

step : Game -> Game
step game =
  if game.losers == [] then
    let
      stepSnakes : Game -> Game
      stepSnakes game =
        game |> updateGameSnakes (List.map stepSnake game.snakes)

      checkCollisions : Game -> Game
      checkCollisions game =
        List.foldl
          (
            \(i, snake) game ->
              if snakeOnBoard game snake then
                game
              else
                {game | losers = game.losers ++ [i]}
          )
          game
          (List.map2 (,) (List.range 0 (List.length game.snakes - 1)) game.snakes)

      -- Check every snake against given apple
      eatApple : Point2D -> Game -> Game
      eatApple apple game =
        let
          whoAteApple =
            List.map
              (
                \snake ->
                  if snakeOnCell apple snake then
                    (
                      True,
                      {snake | additionalLength = snake.additionalLength + 1}
                    )
                  else
                    (False, snake)
              )
              game.snakes

          newApples =
            if List.any first whoAteApple then
              List.filter
                ((/=) apple)
                game.apples
            else
              game.apples

          newSnakes = List.map second whoAteApple
        in
          {game | snakes = newSnakes, apples = newApples}

      eatApples : Game -> Game
      eatApples game =
        List.foldl
          eatApple
          game
          game.apples

      addApples : Game -> Game
      addApples game =
        let
          (needApple, newSeed) =
            Random.step (Random.int 1 10) game.seed

          newApples =
            if needApple == 1 then
              let
                emptySpaces =
                  List.map2
                    (
                      \x y -> case at (Point2D x y) game of
                        None -> (x, y)
                        _ -> (-1, -1)
                    )
                    (List.range 0 (game.width - 1))
                    (List.range 0 (game.height - 1))
                  |> List.filter ((/=) (-1, -1))

                (i, _) = Random.step (Random.int 1 (List.length emptySpaces)) newSeed
                (x, y) = emptySpaces |> List.drop i |> List.head |> withDefault (-1, -1)
              in
                case (x, y) of
                  (-1, -1) -> game.apples
                  (x, y) -> (Point2D x y) :: game.apples
            else
              game.apples
        in
          {game | seed = newSeed, apples = newApples}
    in
      -- Move snakes
      game
      |> stepSnakes
      -- Check collision w/ walls and obstacles
      |> checkCollisions
      -- Check collision w/ each other
      |> eatApples
      |> addApples
  else
    game

input : Int -> Direction -> Game -> Game
input playerNo facing game =
  let
    updateHisSnake i snake =
      if i == playerNo then
        updateSnakePendingFacing facing snake
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
