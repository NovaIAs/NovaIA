```portugal
PROGRAM Pascal PORTUGOL;
TYPE Point = RECORD
  X, Y: INTEGER;
END;

TYPE Node = RECORD
  Value: INTEGER;
  Next: ^Node;
END;

VAR
  P1, P2: Point;
  First, Last, Current: ^Node;
  I: INTEGER;

BEGIN
  P1.X := 10;
  P1.Y := 20;
  P2.X := 30;
  P2.Y := 40;

  First := NIL;
  Last := NIL;

  FOR I := 1 TO 10 DO
  BEGIN
    NEW(Current);
    Current^.Value := I;
    Current^.Next := NIL;

    IF First = NIL THEN
      First := Current
    ELSE
      Last^.Next := Current;

    Last := Current;
  END;

  Current := First;

  WHILE Current <> NIL DO
  BEGIN
    WRITE(Current^.Value);
    Current := Current^.Next;
  END;

  WRITEln;

  Current := First;

  WHILE Current <> NIL DO
  BEGIN
    DISPOSE(Current);
    Current := Current^.Next;
  END;
END.
```

This code creates two points, a linked list of 10 nodes, and then prints the values of the nodes in the linked list.

The code first defines a custom data type called `Point` which has two integer fields, `X` and `Y`. It then defines a custom data type called `Node` which has two fields: `Value` (an integer) and `Next` (a pointer to the next node in the linked list).

The code then declares several variables: two `Point` variables, `P1` and `P2`, and three `Node` variables, `First`, `Last`, and `Current`. It also declares an integer variable `I`.

The code then assigns values to the `X` and `Y` fields of `P1` and `P2`. It then sets the `First` and `Last` variables to `NIL`, which indicates that the linked list is empty.

The code then uses a `FOR` loop to create 10 nodes in the linked list. For each node, it creates a new node using the `NEW` statement, sets the `Value` field of the node to the current value of `I`, and sets the `Next` field of the node to `NIL`.

If the linked list is empty (i.e., `First` is `NIL`), the code sets `First` to point to the new node. Otherwise, the code sets the `Next` field of the last node in the linked list to point to the new node.

The code then updates the `Last` variable to point to the new node.

After the `FOR` loop, the code uses a `WHILE` loop to print the values of the nodes in the linked list. The loop continues until the `Current` variable is `NIL`, which indicates that there are no more nodes in the linked list.

Inside the `WHILE` loop, the code prints the `Value` field of the current node and then updates the `Current` variable to point to the next node in the linked list.

After the `WHILE` loop, the code uses another `WHILE` loop to delete the nodes in the linked list. The loop continues until the `Current` variable is `NIL`, which indicates that there are no more nodes in the linked list.

Inside the `WHILE` loop, the code deletes the current node using the `DISPOSE` statement and then updates the `Current` variable to point to the next node in the linked list.