```scala
// This is a Scala program that implements a simple tic-tac-toe game.

// The following are the classes and objects used in the program:

// The Board class represents the game board. It has a 3x3 grid of cells,
// each of which can be either empty, or contain an X or an O.
class Board {
  private var grid = Array.fill(3, 3)(' ')

  def getCell(row: Int, col: Int): Char = grid(row)(col)

  def setCell(row: Int, col: Int, player: Char): Unit = grid(row)(col) = player

  def isFull: Boolean = grid.forall(_.forall(_ != ' '))

  def hasWinner: Boolean = {
    // Check for a winner in each row.
    for (row <- 0 until 3)
      if (grid(row)(0) != ' ' && grid(row)(0) == grid(row)(1) && grid(row)(1) == grid(row)(2))
        return true

    // Check for a winner in each column.
    for (col <- 0 until 3)
      if (grid(0)(col) != ' ' && grid(0)(col) == grid(1)(col) && grid(1)(col) == grid(2)(col))
        return true

    // Check for a winner in each diagonal.
    if (grid(0)(0) != ' ' && grid(0)(0) == grid(1)(1) && grid(1)(1) == grid(2)(2))
      return true

    if (grid(0)(2) != ' ' && grid(0)(2) == grid(1)(1) && grid(1)(1) == grid(2)(0))
      return true

    // No winner yet.
    false
  }
}

// The Player class represents a player in the game. It has a name and a symbol
// (either 'X' or 'O').
class Player(val name: String, val symbol: Char)

// The Game class manages the game. It has a board and two players. It also has
// a method to start the game and a method to get the current state of the game.
class Game(board: Board, player1: Player, player2: Player) {
  def startGame(): Unit = {
    // Alternate turns between the two players until the game is over.
    var currentPlayer = player1
    while (!board.isFull && !board.hasWinner) {
      // Get the current player's move.
      val move = getMove(currentPlayer)

      // Make the move on the board.
      board.setCell(move.row, move.col, currentPlayer.symbol)

      // Switch to the other player.
      currentPlayer = if (currentPlayer == player1) player2 else player1
    }

    // Print the game state.
    println(board)

    // Print the winner, if any.
    if (board.hasWinner)
      println(currentPlayer.name + " wins!")
    else
      println("It's a tie!")
  }

  def getMove(player: Player): Move = {
    // Prompt the player to enter their move.
    println(player.name + "'s turn. Enter your move (row, column):")

    // Read the player's move from the console.
    val input = scala.io.StdIn.readLine()

    // Parse the player's move.
    val Array(row, col) = input.split(",")

    // Create a Move object from the player's move.
    Move(row.toInt, col.toInt)
  }
}

// The Move class represents a move in the game. It has a row and a column.
case class Move(row: Int, col: Int)

// The main object creates a new game and starts it.
object TicTacToe {
  def main(args: Array[String]): Unit = {
    // Create a new game board.
    val board = new Board

    // Create two new players.
    val player1 = new Player("Player 1", 'X')
    val player2 = new Player("Player 2", 'O')

    // Create a new game.
    val game = new Game(board, player1, player2)

    // Start the game.
    game.startGame()
  }
}
```

This code implements a simple tic-tac-toe game in Scala. The game is played on a 3x3 grid, and each player takes turns placing their symbol (X or O) in an empty cell. The first player to get three of their symbols in a row, column, or diagonal wins the game.

The code is organized into several classes and objects. The `Board` class represents the game board, and the `Player` class represents a player in the game. The `Game` class manages the game, and the `Move` class represents a move in the game.

The `main` method in the `TicTacToe` object creates a new game board, two players, and a new game. It then starts the game by calling the `startGame` method on the `Game` object.

The `startGame` method alternates turns between the two players until the game is over. It does this by calling the `getMove` method on the `Game` object to get the current player's move. It then makes the move on the board by calling the `setCell` method on the `Board` object. Finally, it switches to the other player by calling the `currentPlayer` property on the `Game` object.

The `getMove` method prompts the current player to enter their move. It then reads the player's move from the console and parses it into a `Move` object.

The `Move` class represents a move in the game. It has a row and a column, which represent the cell that the player wants to place their symbol in.

The `Board` class represents the game board. It has a 3x3 grid of cells, each of which can be either empty, or contain an X or an O. The `Board` class has methods to get and set the cell at a given row and column, to check if the board is full, and to check if there is a winner.

The `Player` class represents a player in the game. It has a name and a symbol (either 'X' or 'O').

The `Game` class manages the game. It has a board and two players. It also has methods to start the game and to get the current state of the game.

The `TicTacToe` object creates a new game and starts it.