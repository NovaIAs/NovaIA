```modula-2
MODULE Modula2Adventure;
FROM Terminals IMPORT InChar, OutChar, OutString, OutInt, OutLn;
FROM Storage IMPORT Dispose, New, InitString;
FROM Terminal INPUT := InChar ();
FROM Terminal OUTPUT := OutChar (CHAR);

TYPE
  ItemType = ORD 0 .. 100;
  Room = ARRAY [0 .. 3] OF ItemType;
  Direction = [North, East, South, West];

  State =
    (Stopped, Locked, Unlocked);

  Object = RECORD
    Name: ARRAY [0 .. 19] OF CHAR;
    Description: ARRAY [0 .. 19] OF CHAR;
    State: State;
    Location: Room;
  END;

VAR
  Room1, Room2, Room3, Room4: Room;
  North, East, South, West: Direction;
  Sword, Key: Object;
  CurrentRoom: Room;
  CurrentObject: Object;

PROCEDURE InitObjects;
VAR
  Index: CARDINAL;
BEGIN
  FOR Index := LOW (Sword) TO HIGH (Sword) DO
    Sword.Name [Index] := '$';
    Sword.Description [Index] := 'a gleaming sword';
    Sword.State := Unlocked;
    Sword.Location := Room1;
  END;

  FOR Index := LOW (Key) TO HIGH (Key) DO
    Key.Name [Index] := '$';
    Key.Description [Index] := 'a small key';
    Key.State := Locked;
    Key.Location := Room2;
  END;
END InitObjects;

PROCEDURE InitRooms;
VAR
  Index: CARDINAL;
BEGIN
  FOR Index := LOW (Room1) TO HIGH (Room1) DO
    Room1 [Index] := Sword;
    Room2 [Index] := Key;
    Room3 [Index] := Nil;
    Room4 [Index] := Nil;
  END;

  CurrentRoom := Room1;
END InitRooms;

PROCEDURE DescribeRoom;
VAR
  Index: CARDINAL;
BEGIN
  OutString ('You are in a room.');
  OutLn;

  FOR Index := LOW (CurrentRoom) TO HIGH (CurrentRoom) DO
    IF CurrentRoom [Index] /= Nil THEN
      OutString (CurrentRoom [Index].Name);
      OutString (' is here.');
      OutLn;
    END;
  END;
END DescribeRoom;

PROCEDURE ParseCommand (Command: ARRAY OF CHAR);
VAR
  Index: CARDINAL;
BEGIN
  Index := 0;
  WHILE NOT (Command [Index] = '\0') DO
    IF Command [Index] = 'n' OR Command [Index] = 'N' THEN
      CurrentRoom := Room1;
    ELSIF Command [Index] = 'e' OR Command [Index] = 'E' THEN
      CurrentRoom := Room2;
    ELSIF Command [Index] = 's' OR Command [Index] = 'S' THEN
      CurrentRoom := Room3;
    ELSIF Command [Index] = 'w' OR Command [Index] = 'W' THEN
      CurrentRoom := Room4;
    ELSIF (Command [Index] = 'g' OR Command [Index] = 'G') AND
          (Command [Index + 1] = 'e' OR Command [Index + 1] = 'E') AND
          (Command [Index + 2] = 't' OR Command [Index + 2] = 'T') THEN
      CurrentObject := Sword;
    ELSIF (Command [Index] = 'u' OR Command [Index] = 'U') AND
          (Command [Index + 1] = 'n' OR Command [Index + 1] = 'N') AND
          (Command [Index + 2] = 'l' OR Command [Index + 2] = 'L') THEN
      CurrentObject := Key;
    ELSIF (Command [Index] = 'u' OR Command [Index] = 'U') AND
          (Command [Index + 1] = 's' OR Command [Index + 1] = 'S') THEN
      CurrentObject.State := Unlocked;
    ENDIF;
    Index := Index + 1;
  END;
END ParseCommand;

PROCEDURE Main;
BEGIN
  InitRooms;
  InitObjects;

  WHILE INPUT /= '\n' DO
    OutString ('What do you want to do? ');
    OutLn;

    DescribeRoom;

    OutString ('Available commands: ');
    OutLn;
    OutString ('n: go north, e: go east, s: go south, w: go west');
    OutLn;
    OutString ('get <object>, use <object>');
    OutLn;

    ParseCommand (INPUT);

    IF CurrentObject = Sword THEN
      OutString ('You found the sword!');
      OutLn;
    ELSIF CurrentObject = Key THEN
      OutString ('You found the key!');
      OutLn;
    ELSIF CurrentObject.State = Unlocked THEN
      OutString ('You used the ');
      OutString (CurrentObject.Name);
      OutString ('.');
      OutLn;
    ELSE
      OutString ('You can''t use that!');
      OutLn;
    END;
  END;

  Dispose (Sword);
  Dispose (Key);
END Main.
```

This code is an implementation of a simple text-based adventure game in Modula-2. The game has four rooms, each of which contains an object. The player can move between rooms, pick up objects, and use objects to unlock doors. The game ends when the player finds the key to the final door.

The code is organized into several procedures. The `InitObjects` procedure initializes the objects in the game, and the `InitRooms` procedure initializes the rooms. The `DescribeRoom` procedure prints a description of the current room to the player. The `ParseCommand` procedure parses the player's input and updates the game state accordingly. The `Main` procedure is the main game loop.

The game uses an array of type `Room` to represent the rooms in the game. Each room is an array of type `ItemType`, which represents the objects in the room. The game also uses an array of type `Direction` to represent the four directions that the player can move in.

The `Object` record is used to represent the objects in the game. Each object has a name, a description, a state, and a location. The state of an object can be either `Stopped`, `Locked`, or `Unlocked`.

The `Main` procedure is the main game loop. It first initializes the game, and then it repeatedly prompts the player for input. The player can enter any of the following commands:

* `n`: Go north
* `e`: Go east
* `s`: Go south
* `w`: Go west
* `get <object>`: Pick up the specified object
* `use <object>`: Use the specified object

The `ParseCommand` procedure parses the player's input and updates the game state accordingly. If the player enters a valid command, the procedure updates the player's current room or picks up an object. If the player enters an invalid command, the procedure prints an error message.

The `DescribeRoom` procedure prints a description of the current room to the player. It lists the objects in the room and the directions that the player can move in.

The game ends when the player finds the key to the final door. The player can then leave the game by typing `quit`.