```lua
-- This Lua script is a complex and differentiated code that demonstrates various features of the language.

-- Define a custom print function to colorize output
printc = function(text, color)
    io.write("\27["..color.."m"..text.."\27[0m")
end

-- Create a table of colors for easy reference
colors = {
    ["black"]    = 30,
    ["red"]      = 31,
    ["green"]    = 32,
    ["yellow"]   = 33,
    ["blue"]     = 34,
    ["magenta"]  = 35,
    ["cyan"]     = 36,
    ["white"]    = 37,

    ["bright black"]    = 90,
    ["bright red"]      = 91,
    ["bright green"]    = 92,
    ["bright yellow"]   = 93,
    ["bright blue"]     = 94,
    ["bright magenta"]  = 95,
    ["bright cyan"]     = 96,
    ["bright white"]    = 97,
}

-- Create a table to store the current state of the game
game = {
    board = {
        {" ", " ", " "},
        {" ", " ", " "},
        {" ", " ", " "}
    },
    turn = "X",
    winner = nil
}

-- Function to print the current state of the game board
function print_board()
    printc("\n   a b c\n")
    for i, row in ipairs(game.board) do
        printc(i .. "  ", "bright white")
        for j, cell in ipairs(row) do
            printc("  " .. cell, colors[cell])
        end
        print()
    end
    print()
end

-- Function to check if there is a winner
function check_winner()
    -- Check rows
    for i = 1, 3 do
        if game.board[i][1] ~= " " and game.board[i][1] == game.board[i][2] and game.board[i][2] == game.board[i][3] then
            return game.board[i][1]
        end
    end

    -- Check columns
    for j = 1, 3 do
        if game.board[1][j] ~= " " and game.board[1][j] == game.board[2][j] and game.board[2][j] == game.board[3][j] then
            return game.board[1][j]
        end
    end

    -- Check diagonals
    if game.board[1][1] ~= " " and game.board[1][1] == game.board[2][2] and game.board[2][2] == game.board[3][3] then
        return game.board[1][1]
    end
    if game.board[1][3] ~= " " and game.board[1][3] == game.board[2][2] and game.board[2][2] == game.board[3][1] then
        return game.board[1][3]
    end

    -- Check for a tie
    if not game.winner and not has_empty_cells() then
        return "tie"
    end

    return nil
end

-- Function to check if there are any empty cells left on the board
function has_empty_cells()
    for i = 1, 3 do
        for j = 1, 3 do
            if game.board[i][j] == " " then
                return true
            end
        end
    end
    return false
end

-- Function to get the player's move
function get_move()
    printc("It's "..game.turn.."'s turn. Enter your move (e.g. a1): ", colors["bright green"])
    local move = io.read():lower()

    -- Validate the move
    if move:match("^([a-c])([1-3])$") then
        local row = move:byte(2) - 96
        local col = move:byte(1) - 97
        if game.board[row][col] == " " then
            return row, col
        end
    end

    printc("Invalid move. Please try again.", colors["bright red"])
    return get_move()
end

-- Function to make a move on the board
function make_move(row, col)
    game.board[row][col] = game.turn
end

-- Function to switch turns between players
function switch_turns()
    game.turn = game.turn == "X" and "O" or "X"
end

-- Main game loop
while not game.winner do
    print_board()

    -- Get the player's move
    local row, col = get_move()

    -- Make the move
    make_move(row, col)

    -- Check for a winner
    game.winner = check_winner()

    -- Switch turns
    switch_turns()
end

-- Print the result of the game
if game.winner == "tie" then
    printc("It's a tie!", colors["bright yellow"])
else
    printc("Congratulations, "..game.winner.." wins!", colors["bright green"])
end
```

This code is a complex and differentiated Lua script that demonstrates various features of the language, including:

* Custom print function for colorized output
* Use of a table to store game state
* Function to print the current state of the game board
* Function to check for a winner
* Function to check if there are any empty cells left on the board
* Function to get the player's move
* Function to make a move on the board
* Function to switch turns between players
* Main game loop

The script is designed to be a tic-tac-toe game where two players take turns making moves on a 3x3 board. The game checks for a winner after each move and switches turns between players. The script also handles invalid moves and prints the result of the game when it ends.

The code is well-commented and organized, making it easy to understand and modify. It demonstrates the use of various Lua constructs, such as tables, functions, loops, and conditional statements.