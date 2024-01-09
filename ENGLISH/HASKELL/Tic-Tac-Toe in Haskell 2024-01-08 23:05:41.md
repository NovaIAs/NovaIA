```haskell
import Control.Monad (liftM, replicateM, unless)
import Data.List (find, sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A type representing a game of tic-tac-toe.
data TicTacToe = TicTacToe
  { -- | The current state of the board.
    board :: [[Char]],
    -- | The player who is currently moving.
    currentPlayer :: Char,
    -- | The set of all possible winning positions.
    winningPositions :: Set.Set [Int]
  }

-- | Create a new game of tic-tac-toe.
newGame :: TicTacToe
newGame = TicTacToe
  { board = replicateM 3 (replicateM 3 ' '),
    currentPlayer = 'X',
    winningPositions = winningPositions3x3
  }

-- | The set of all possible winning positions in a 3x3 tic-tac-toe game.
winningPositions3x3 :: Set.Set [Int]
winningPositions3x3 = Set.fromList
  [ [1, 2, 3], [4, 5, 6], [7, 8, 9],
    [1, 4, 7], [2, 5, 8], [3, 6, 9],
    [1, 5, 9], [3, 5, 7]
  ]

-- | Make a move in a game of tic-tac-toe.
makeMove :: TicTacToe -> Int -> TicTacToe
makeMove (TicTacToe board player winningPositions) position =
  let (newBoard, newPlayer) = makeMove' board player position
  in TicTacToe newBoard newPlayer winningPositions

-- | Helper function for `makeMove`.
makeMove' :: [[Char]] -> Char -> Int -> ([[Char]], Char)
makeMove' board player position =
  let (row, col) = indexToRowCol position
  in (boardWithMove board row col player, nextPlayer player)

-- | Get the row and column indices of a position on the board.
indexToRowCol :: Int -> (Int, Int)
indexToRowCol position =
  let row = (position - 1) `div` 3
      col = (position - 1) `mod` 3
  in (row, col)

-- | Get the player who is next to move.
nextPlayer :: Char -> Char
nextPlayer 'X' = 'O'
nextPlayer 'O' = 'X'

-- | Check if a game of tic-tac-toe is over.
isGameOver :: TicTacToe -> Bool
isGameOver (TicTacToe board _ winningPositions) =
  any (\positions -> all (`elem` boardPositions) positions) winningPositions
 where
  boardPositions = concat board

-- | Get the winner of a game of tic-tac-toe.
getWinner :: TicTacToe -> Maybe Char
getWinner (TicTacToe board _ winningPositions) =
  let winningPosition = find (\positions -> all (`elem` boardPositions) positions) winningPositions
  in fmap head winningPosition

-- | Pretty-print a game of tic-tac-toe.
showTicTacToe :: TicTacToe -> String
showTicTacToe (TicTacToe board _ _) =
  unlines [showRow row | row <- board]
 where
  showRow = intercalate " | " . map show

-- | The main function.
main :: IO ()
main = do
  putStrLn "Welcome to tic-tac-toe!"
  putStrLn "Enter a position (1-9) to make a move, or 'q' to quit."
  gameLoop newGame

-- | The main game loop.
gameLoop :: TicTacToe -> IO ()
gameLoop game = do
  unless (isGameOver game) $ do
    putStrLn (showTicTacToe game)
    putStrLn (currentPlayer game ++ "'s turn:")
    position <- getLine
    case position of
      "q" -> putStrLn "Goodbye!"
      _ -> do
        let newGame = makeMove game (read position :: Int)
        gameLoop newGame
  let winner = getWinner game
  putStrLn (showTicTacToe game)
  case winner of
    Just 'X' -> putStrLn "X wins!"
    Just 'O' -> putStrLn "O wins!"
    Nothing -> putStrLn "It's a tie!"
```

This code implements a game of tic-tac-toe in Haskell. It uses a data type called `TicTacToe` to represent the game state, which includes the current board, the current player, and the set of all possible winning positions.

The `newGame` function creates a new game of tic-tac-toe, and the `makeMove` function makes a move in the game. The `isGameOver` function checks if the game is over, and the `getWinner` function returns the winner of the game, if any. The `showTicTacToe` function pretty-prints the game state, and the `main` function is the entry point to the program.

The `gameLoop` function is the main game loop, which repeatedly prompts the player to make a move, updates the game state, and checks for a winner. The loop terminates when the game is over, and the winner is displayed.