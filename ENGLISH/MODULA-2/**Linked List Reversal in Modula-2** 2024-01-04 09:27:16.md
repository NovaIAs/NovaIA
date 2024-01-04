```modula-2
MODULE ComplexCode;

FROM FormatString IMPORT Printf;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
    NODE = RECORD
        Value: INTEGER;
        Next: NODE;
    END;

PROCEDURE CreateList(VAR First: NODE; VAR Last: NODE);
VAR
    NewNode: NODE;
BEGIN
    NewNode := NODE: (Value => 1, Next => NIL);
    First := NewNode;
    Last := NewNode;

    FOR I := 2 TO 10 DO
        NewNode := NODE: (Value => I, Next => NIL);
        Last.Next := NewNode;
        Last := NewNode;
    END;
END CreateList;

PROCEDURE PrintList(First: NODE);
VAR
    Current: NODE;
BEGIN
    Current := First;
    WHILE Current <> NIL DO
        Printf("Value: %d\n", [Current.Value]);
        Current := Current.Next;
    END;
END PrintList;

PROCEDURE ReverseList(VAR First: NODE): NODE;
VAR
    Previous: NODE;
    Current: NODE;
    Next: NODE;
BEGIN
    Previous := NIL;
    Current := First;
    WHILE Current <> NIL DO
        Next := Current.Next;
        Current.Next := Previous;
        Previous := Current;
        Current := Next;
    END;
    First := Previous;
    RETURN First;
END ReverseList;

VAR
    Head: NODE;
    Tail: NODE;
BEGIN
    CreateList(Head, Tail);
    Printf("Original List:\n");
    PrintList(Head);
    WriteString("\n");

    Head := ReverseList(Head);
    Printf("Reversed List:\n");
    PrintList(Head);
    WriteLn;
END ComplexCode.
```

**Explanation:**

This code demonstrates a complex and unique operation using a linked list in Modula-2:

1. **CreateList Procedure:**
   - It creates a linked list of 10 nodes, where each node contains an integer value from 1 to 10.
   - The first node is assigned to the variable `Head`, and the last node is assigned to the variable `Tail`.

2. **PrintList Procedure:**
   - It prints the values of all the nodes in the linked list, starting from the `Head` node.

3. **ReverseList Procedure:**
   - It reverses the order of the nodes in the linked list.
   - It does this by iterating through the list and changing the `Next` pointer of each node to point to the previous node.
   - After the iteration, the `Head` node will point to the last node, and the `Tail` node will point to the first node.

4. **Main Module:**
   - It calls the `CreateList` procedure to create the initial linked list.
   - It prints the original list using the `PrintList` procedure.
   - It then calls the `ReverseList` procedure to reverse the linked list.
   - Finally, it prints the reversed list using the `PrintList` procedure.

This code showcases a complex and differentiated operation, which is unlikely to be repeated in the same way again. It demonstrates the use of linked lists, dynamic memory allocation, and pointer manipulation in Modula-2.