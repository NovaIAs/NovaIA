```cool
class Game {
  board : String;
  score : Int;
  current_player : String;

  constructor (board, score, current_player) {
    this.board = board;
    this.score = score;
    this.current_player = current_player;
  }

  switchPlayer() {
    if (this.current_player == "X") {
      this.current_player = "O";
    } else {
      this.current_player = "X";
    }
  }

  makeMove(x, y) {
    if (this.board[x][y] == " ") {
      this.board[x][y] = this.current_player;
      this.switchPlayer();
    }
  }

  checkForWinner() {
    // Check for horizontal wins
    for (i = 0; i < 3; i++) {
      if (this.board[i][0] != " " && this.board[i][0] == this.board[i][1] && this.board[i][0] == this.board[i][2]) {
        return this.board[i][0];
      }
    }

    // Check for vertical wins
    for (i = 0; i < 3; i++) {
      if (this.board[0][i] != " " && this.board[0][i] == this.board[1][i] && this.board[0][i] == this.board[2][i]) {
        return this.board[0][i];
      }
    }

    // Check for diagonal wins
    if (this.board[0][0] != " " && this.board[0][0] == this.board[1][1] && this.board[0][0] == this.board[2][2]) {
      return this.board[0][0];
    }
    if (this.board[0][2] != " " && this.board[0][2] == this.board[1][1] && this.board[0][2] == this.board[2][0]) {
      return this.board[0][2];
    }

    // Check for a tie
    if (!this.board.includes(" ")) {
      return "Tie";
    }

    // No winner yet
    return null;
  }
}
```

This code implements a simple tic-tac-toe game in COOL. The `Game` class has three attributes: `board`, `score`, and `current_player`. The `board` attribute is a 3x3 array of strings representing the state of the game board. The `score` attribute is an integer representing the current score of the game. The `current_player` attribute is a string representing the player who is currently taking their turn.

The `constructor` method initializes a new `Game` object with the given board, score, and current player.

The `switchPlayer` method switches the current player from "X" to "O" or vice versa.

The `makeMove` method takes two arguments, `x` and `y`, representing the coordinates of the square on the board that the player wants to place their piece. The method checks if the square is empty and if so, it places the player's piece on the square and switches the current player.

The `checkForWinner` method checks the board for a winner. It first checks for horizontal wins, then vertical wins, and then diagonal wins. If a winner is found, the method returns the winner's symbol. If no winner is found, the method checks for a tie. If there is a tie, the method returns "Tie". Otherwise, the method returns `null`.

To use this code, you would first create a new `Game` object. Then, you would call the `makeMove` method to place pieces on the board. After each move, you would call the `checkForWinner` method to check for a winner or a tie. You would continue playing the game until a winner is found or a tie occurs.