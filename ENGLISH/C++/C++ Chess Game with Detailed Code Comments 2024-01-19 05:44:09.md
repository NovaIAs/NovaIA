Here is a C++ code that implements the classic game of chess, with detailed comments explaining the code:

```c++
// Include necessary libraries
#include <iostream>
#include <vector>
#include <map>

// Define the chess board size
const int BOARD_SIZE = 8;

// Define the player colors
enum PlayerColor {
  WHITE,
  BLACK
};

// Define the piece types
enum PieceType {
  PAWN,
  ROOK,
  BISHOP,
  KNIGHT,
  QUEEN,
  KING
};

// Define the piece movement directions
map<PieceType, vector<pair<int, int>>> piece_directions = {
  {PAWN, {{1, 0}, {2, 0}}},
  {ROOK, {{0, 1}, {0, -1}, {1, 0}, {-1, 0}}},
  {BISHOP, {{1, 1}, {1, -1}, {-1, 1}, {-1, -1}}},
  {KNIGHT, {{1, 2}, {2, 1}, {2, -1}, {1, -2}, {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}}},
  {QUEEN, {{0, 1}, {0, -1}, {1, 0}, {-1, 0}, {1, 1}, {1, -1}, {-1, 1}, {-1, -1}}},
  {KING, {{0, 1}, {0, -1}, {1, 0}, {-1, 0}, {1, 1}, {1, -1}, {-1, 1}, {-1, -1}}}
};

// Define the chess piece
class Piece {
public:
  PieceType type;
  PlayerColor color;
  Piece(PieceType type, PlayerColor color) : type(type), color(color) {}
};

// Define the chess square
class Square {
public:
  Piece* piece;
  bool is_occupied() { return piece != nullptr; }
  Square() : piece(nullptr) {}
};

// Define the chess board
class Board {
public:
  Square squares[BOARD_SIZE][BOARD_SIZE];

  // Initialize the board with the starting pieces
  void initialize() {
    // Place the pawns
    for (int i = 0; i < BOARD_SIZE; i++) {
      squares[1][i].piece = new Piece(PAWN, WHITE);
      squares[6][i].piece = new Piece(PAWN, BLACK);
    }

    // Place the rooks
    squares[0][0].piece = new Piece(ROOK, WHITE);
    squares[0][7].piece = new Piece(ROOK, WHITE);
    squares[7][0].piece = new Piece(ROOK, BLACK);
    squares[7][7].piece = new Piece(ROOK, BLACK);

    // Place the bishops
    squares[0][2].piece = new Piece(BISHOP, WHITE);
    squares[0][5].piece = new Piece(BISHOP, WHITE);
    squares[7][2].piece = new Piece(BISHOP, BLACK);
    squares[7][5].piece = new Piece(BISHOP, BLACK);

    // Place the knights
    squares[0][1].piece = new Piece(KNIGHT, WHITE);
    squares[0][6].piece = new Piece(KNIGHT, WHITE);
    squares[7][1].piece = new Piece(KNIGHT, BLACK);
    squares[7][6].piece = new Piece(KNIGHT, BLACK);

    // Place the queens
    squares[0][3].piece = new Piece(QUEEN, WHITE);
    squares[7][3].piece = new Piece(QUEEN, BLACK);

    // Place the kings
    squares[0][4].piece = new Piece(KING, WHITE);
    squares[7][4].piece = new Piece(KING, BLACK);
  }
};

// Define the chess game
class Game {
public:
  Board board;
  PlayerColor current_player;

  // Constructor
  Game() : current_player(WHITE) {}

  // Make a move
  bool make_move(string move) {
    // Parse the move string
    int from_row = move[0] - '1';
    int from_col = move[1] - 'a';
    int to_row = move[2] - '1';
    int to_col = move[3] - 'a';

    // Check if the move is valid
    if (!is_valid_move(from_row, from_col, to_row, to_col)) {
      return false;
    }

    // Make the move
    Piece* piece = board.squares[from_row][from_col].piece;
    board.squares[from_row][from_col].piece = nullptr;
    board.squares[to_row][to_col].piece = piece;

    // Change the current player
    current_player = (current_player == WHITE) ? BLACK : WHITE;

    return true;
  }

  // Check if a move is valid
  bool is_valid_move(int from_row, int from_col, int to_row, int to_col) {
    // Check if the move is within the board boundaries
    if (from_row < 0 || from_row >= BOARD_SIZE || from_col < 0 || from_col >= BOARD_SIZE ||
        to_row < 0 || to_row >= BOARD_SIZE || to_col < 0 || to_col >= BOARD_SIZE) {
      return false;
    }

    // Check if the square to move to is occupied by a piece of the same color
    if (board.squares[to_row][to_col].is_occupied() &&
        board.squares[to_row][to_col].piece->color == current_player) {
      return false;
    }

    // Check if the move is allowed for the piece
    Piece* piece = board.squares[from_row][from_col].piece;
    vector<pair<int, int>> directions = piece_directions[piece->type];
    for (auto direction : directions) {
      int row = from_row + direction.first;
      int col = from_col + direction.second;
      if (row == to_row && col == to_col) {
        return true;
      }
    }

    return false;
  }

  // Check if the game is over
  bool is_game_over() {
    // Check if the king of the current player is in checkmate
    Square* king_square = find_king_square();
    if (king_square == nullptr) {
      return true; // King is not found, so the game is over
    }

    // Check if the king is in check
    if (is_king_in_check(king_square->piece)) {
      // Check if there is a valid move to get out of check
      for (int i = 0; i < BOARD_SIZE; i++) {
        for (int j = 0; j < BOARD_SIZE; j++) {
          if (board.squares[i][j].is_occupied() && board.squares[i][j].piece->color == current_player) {
            for (auto direction : piece_directions[board.squares[i][j].piece->type