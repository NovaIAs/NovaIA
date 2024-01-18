**Creating a Tic-Tac-Toe Game in Smalltalk**

```smalltalk
Object subclass: TTTGame [
  | theBoard |

  class methods [
    create [ | TTTGame |
      TTTGame new: 5 ] ]

  instance methods [
    new: size [ | TTTGame |
      TTTGame superman := super new.
      TTTGame theBoard := Array new: size.
      ^TTTGame ]

    printBoard [
      theBoard do: [ :row |
        Transcript cr; row do: [ :aSpot | Transcript show: aSpot ]. ] ].

    playMove: aMove [
      aMove x: 1 y: 1 ifTrue: [
        theBoard at: 1 put: 'X' ]
      ifFalse: [
        theBoard at: 2 put: 'X' ].
      self printBoard.

      (theBoard at: 1) = 'X' and: [
        (theBoard at: 2) = 'X' and: [
          (theBoard at: 3) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 4) = 'X' and: [
        (theBoard at: 5) = 'X' and: [
          (theBoard at: 6) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 7) = 'X' and: [
        (theBoard at: 8) = 'X' and: [
          (theBoard at: 9) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 1) = 'X' and: [
        (theBoard at: 4) = 'X' and: [
          (theBoard at: 7) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 2) = 'X' and: [
        (theBoard at: 5) = 'X' and: [
          (theBoard at: 8) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 3) = 'X' and: [
        (theBoard at: 6) = 'X' and: [
          (theBoard at: 9) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 1) = 'X' and: [
        (theBoard at: 5) = 'X' and: [
          (theBoard at: 9) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ].

      (theBoard at: 3) = 'X' and: [
        (theBoard at: 5) = 'X' and: [
          (theBoard at: 7) = 'X' ] ]
      ifTrue: [
        Transcript cr; Transcript show: "You win!" ] ] ] ] ]

TTTGame create playMove: (Move create x: 1 y: 1).
```

**Explanation:**

1. **Class Definition:** The code starts by defining a class called `TTTGame` that represents a Tic-Tac-Toe game.

2. **Class Methods:** The `TTTGame` class has a single class method called `create` that initializes a new game with a specified size. In this case, it always creates a game of size 5.

3. **Instance Methods:** The `TTTGame` class has several instance methods:

   - **`new: size`:** This method creates a new instance of the `TTTGame` class with the specified size. It sets up the game board and initializes the instance variables.
   - **`printBoard`:** This method prints the current state of the game board to the console.
   - **`playMove: aMove`:** This method takes a `Move` object as input and updates the game board accordingly. It then checks for a winner and prints the result to the console.

4. **Creating an Instance of `TTTGame`:** The code then creates an instance of the `TTTGame` class and plays a move at position (1, 1) using the `playMove:` method.

5. **Checking for a Winner:** The code checks for a winner after each move by checking if there are three consecutive 'X's in any row, column, or diagonal. If a winner is found, it prints a message to the console.

6. **Playing the Game:** To play the game, you can repeatedly call the `playMove:` method to make moves and check for a winner. You can also use the `printBoard` method to display the current state of the game board.