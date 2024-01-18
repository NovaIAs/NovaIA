```haskell
-- This is a Haskell program that implements a simple tic-tac-toe game.

-- The `data` keyword is used to define a new data type. In this case, we are
-- defining a data type called `Board` that represents the tic-tac-toe board.
data Board = Empty | X | O
    deriving (Eq, Ord, Show)

-- The `instance` keyword is used to define an instance of a typeclass for a
-- particular data type. In this case, we are defining an instance of the
-- `Show` typeclass for the `Board` data type. This means that we can use the
-- `show` function to convert a `Board` value into a string.
instance Show Board where
    show (Empty) = " "
    show X       = "X"
    show O       = "O"

-- The `main` function is the entry point for the program.
main :: IO ()
main = do
    -- Create a new, empty tic-tac-toe board.
    board <- newBoard

    -- Play the game until a winner is determined or there is a tie.
    playGame board

-- The `newBoard` function creates a new, empty tic-tac-toe board.
newBoard :: IO Board
newBoard = return (replicate 9 Empty)

-- The `playGame` function plays a game of tic-tac-toe on the given board.
playGame :: Board -> IO ()
playGame board = do
    -- Get the move from the first player.
    move1 <- getMove "Player 1"

    -- Make the move on the board.
    let board1 = makeMove board move1 X

    -- Check if the game is over.
    result1 <- checkGameOver board1

    -- If the game is over, print the result and return.
    if result1 /= Nothing then do
        putStrLn result1
        return
    else do
        -- Get the move from the second player.
        move2 <- getMove "Player 2"

        -- Make the move on the board.
        let board2 = makeMove board1 move2 O

        -- Check if the game is over.
        result2 <- checkGameOver board2

        -- If the game is over, print the result and return.
        if result2 /= Nothing then do
            putStrLn result2
            return
        else do
            -- Continue playing the game.
            playGame board2

-- The `getMove` function gets a move from the given player.
getMove :: String -> IO Int
getMove player = do
    putStrLn $ player ++ ", it's your turn. Enter a move (1-9):"
    move <- getLine
    case move of
        "1" -> return 0
        "2" -> return 1
        "3" -> return 2
        "4" -> return 3
        "5" -> return 4
        "6" -> return 5
        "7" -> return 6
        "8" -> return 7
        "9" -> return 8
        _ -> do
            putStrLn "Invalid move. Please enter a move (1-9):"
            getMove player

-- The `makeMove` function makes a move on the given board.
makeMove :: Board -> Int -> Board -> Board
makeMove board move player =
    let board' = take move board ++ [player] ++ drop (move + 1) board
    in board'

-- The `checkGameOver` function checks if the game is over.
checkGameOver :: Board -> IO (Maybe String)
checkGameOver board = do
    -- Check if there is a winner.
    winner <- getWinner board

    -- If there is a winner, return the winner's name.
    if winner /= Nothing then do
        return $ "Player " ++ winner ++ " wins!"
    else do
        -- Check if there is a tie.
        tie <- getTie board

        -- If there is a tie, return "Tie game".
        if tie then do
            return "Tie game."
        else do
            -- The game is not over yet. Return Nothing.
            return Nothing

-- The `getWinner` function gets the winner of the game.
getWinner :: Board -> IO (Maybe String)
getWinner board = do
    -- Check if there is a winner in any of the rows.
