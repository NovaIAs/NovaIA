```javascript
// Establish a global variable to store the current game state.
const gameState = {
  board: [
    ["", "", ""],
    ["", "", ""],
    ["", "", ""],
  ],
  currentPlayer: "X",
  winner: null,
};

// Function to handle player's move and update the game state.
const makeMove = (row, column) => {
  // Check if the selected cell is already occupied.
  if (gameState.board[row][column] !== "") {
    alert("Invalid move. Please select an empty cell.");
    return;
  }

  // Update the game state with the current player's move.
  gameState.board[row][column] = gameState.currentPlayer;

  // Check if the current player has won the game.
  if (checkForWin(gameState.board)) {
    gameState.winner = gameState.currentPlayer;
    alert(`${gameState.currentPlayer} wins!`);
    return;
  }

  // Switch the active player for the next turn.
  gameState.currentPlayer = gameState.currentPlayer === "X" ? "O" : "X";
};

// Function to check if there is a winner in the current game state.
const checkForWin = (board) => {
  // Check for horizontal wins.
  for (let i = 0; i < 3; i++) {
    if (
      board[i][0] !== "" &&
      board[i][0] === board[i][1] &&
      board[i][1] === board[i][2]
    ) {
      return true;
    }
  }

  // Check for vertical wins.
  for (let j = 0; j < 3; j++) {
    if (
      board[0][j] !== "" &&
      board[0][j] === board[1][j] &&
      board[1][j] === board[2][j]
    ) {
      return true;
    }
  }

  // Check for diagonal wins.
  if (
    board[0][0] !== "" &&
    board[0][0] === board[1][1] &&
    board[1][1] === board[2][2]
  ) {
    return true;
  }

  if (
    board[0][2] !== "" &&
    board[0][2] === board[1][1] &&
    board[1][1] === board[2][0]
  ) {
    return true;
  }

  // If no winner yet, return false.
  return false;
};

// Function to create a visual representation of the game board.
const displayBoard = () => {
  const boardElement = document.getElementById("board");
  boardElement.innerHTML = "";

  for (let i = 0; i < 3; i++) {
    const rowElement = document.createElement("div");
    rowElement.classList.add("row");

    for (let j = 0; j < 3; j++) {
      const cellElement = document.createElement("div");
      cellElement.classList.add("cell");
      cellElement.textContent = gameState.board[i][j];

      // Add event listener to handle player's move.
      cellElement.addEventListener("click", () => {
        makeMove(i, j);
        displayBoard();
      });

      rowElement.appendChild(cellElement);
    }

    boardElement.appendChild(rowElement);
  }
};

// Function to reset the game state and start a new game.
const resetGame = () => {
  gameState.board = [
    ["", "", ""],
    ["", "", ""],
    ["", "", ""],
  ];
  gameState.currentPlayer = "X";
  gameState.winner = null;
  displayBoard();
};

// Function to initialize the game and set up event listeners.
const initializeGame = () => {
  const boardElement = document.getElementById("board");
  const resetButton = document.getElementById("reset-button");

  // Add event listener to handle player's moves.
  boardElement.addEventListener("click", (event) => {
    const cellElement = event.target;
    if (cellElement.classList.contains("cell")) {
      const row = cellElement.parentElement.rowIndex;
      const column = cellElement.cellIndex;
      makeMove(row, column);
      displayBoard();
    }
  });

  // Add event listener to handle reset button.
  resetButton.addEventListener("click", () => {
    resetGame();
  });
};

// Call the initializeGame function to start the game.
initializeGame();
```

Explanation:

1. **Global Variable `gameState`:** This object stores the current state of the game, including the game board, the active player, and the winner.

2. **Function `makeMove`:** This function is responsible for updating the game state when a player makes a move. It checks if the selected cell is empty, updates the game board, and switches the active player.

3. **Function `checkForWin`:** This function checks if there is a winner in the current game state. It looks for horizontal, vertical, and diagonal wins on the game board.

4. **Function `displayBoard`:** This function creates a visual representation of the game board by dynamically generating HTML elements. It updates the text content of each cell with the corresponding value from the game board stored in `gameState.board`.

5. **Function `resetGame`:** This function is called to reset the game state and start a new game. It initializes the game board, sets the current player to "X", and clears the winner information.

6. **Function `initializeGame`:** This function sets up the event listeners for handling player moves and the reset button. It calls the `displayBoard` function to display the initial empty game board.

7. **Initialization:** The `initializeGame` function is called to start the game and set up event listeners.