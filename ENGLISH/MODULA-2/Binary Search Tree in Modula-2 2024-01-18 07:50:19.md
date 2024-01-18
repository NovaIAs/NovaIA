```modula-2

MODULE BinarySearchTree;

FROM Terminal IMPORT WriteString, WriteLn, ReadLine;
FROM Strings IMPORT StringToString;

TYPE
  Node = RECORD
    Value: INTEGER;
    Left: REF Node;
    Right: REF Node;
  END;

VAR
  Root: REF Node;

PROCEDURE Insert(Value: INTEGER);
VAR
  NewNode: REF Node;
  CurrentNode, ParentNode: REF Node;
BEGIN
  NEW(NewNode);
  NewNode^.Value := Value;
  NewNode^.Left := NIL;
  NewNode^.Right := NIL;

  IF Root = NIL THEN
    Root := NewNode
  ELSE
    CurrentNode := Root;
    WHILE CurrentNode <> NIL DO
      ParentNode := CurrentNode;
      IF Value < CurrentNode^.Value THEN
        CurrentNode := CurrentNode^.Left
      ELSE
        CurrentNode := CurrentNode^.Right
      END;
    END;
    IF Value < ParentNode^.Value THEN
      ParentNode^.Left := NewNode
    ELSE
      ParentNode^.Right := NewNode
    END;
  END;
END Insert;

PROCEDURE InorderTraversal(CurrentNode: REF Node);
BEGIN
  IF CurrentNode <> NIL THEN
    InorderTraversal(CurrentNode^.Left);
    WriteString(StringToString(CurrentNode^.Value));
    WriteLn;
    InorderTraversal(CurrentNode^.Right);
  END;
END InorderTraversal;

PROCEDURE PreorderTraversal(CurrentNode: REF Node);
BEGIN
  IF CurrentNode <> NIL THEN
    WriteString(StringToString(CurrentNode^.Value));
    WriteLn;
    PreorderTraversal(CurrentNode^.Left);
    PreorderTraversal(CurrentNode^.Right);
  END;
END PreorderTraversal;

PROCEDURE PostorderTraversal(CurrentNode: REF Node);
BEGIN
  IF CurrentNode <> NIL THEN
    PostorderTraversal(CurrentNode^.Left);
    PostorderTraversal(CurrentNode^.Right);
    WriteString(StringToString(CurrentNode^.Value));
    WriteLn;
  END;
END PostorderTraversal;

BEGIN
  WriteString("Enter integers (type 'q' to quit): ");
  WHILE TRUE DO
    VAR Input: ARRAY[1 .. 80] OF CHAR;
    BEGIN
      ReadLine(Input);
      IF Input[1] = 'q' THEN
        EXIT
      ELSE
        Insert(StringToInt(Input))
      END;
    END;
  END;

  WriteString("Inorder traversal: ");
  InorderTraversal(Root);

  WriteString("Preorder traversal: ");
  PreorderTraversal(Root);

  WriteString("Postorder traversal: ");
  PostorderTraversal(Root);
END BinarySearchTree.

```

This code implements a binary search tree in Modula-2. A binary search tree is a data structure that stores data in a way that allows for efficient searching, insertion, and deletion. The code includes the following main components:

1. **Node Record:** The Node record defines the structure of each node in the binary search tree. Each node has three fields: Value, Left, and Right. The Value field stores the data value of the node, while the Left and Right fields point to the left and right child nodes, respectively.

2. **Insert Procedure:** The Insert procedure is used to insert a new data value into the binary search tree. It takes the value to be inserted as an argument and adds it to the tree in the appropriate location. The algorithm recursively traverses the tree, comparing the value to be inserted with the values of the existing nodes, until it finds the correct place to insert the new node.

3. **InorderTraversal, PreorderTraversal, and PostorderTraversal Procedures:** These procedures are used to traverse the binary search tree in different orders. Inorder traversal visits the nodes in the left subtree, then the current node, and then the right subtree. Preorder traversal visits the current node, then the left subtree, and then the right subtree. Postorder traversal visits the left subtree, then the right subtree, and then the current node.

4. **Main Procedure:** The main procedure is the entry point of the program. It prompts the user to enter integers, and it uses the Insert procedure to insert each integer into the binary search tree. After all integers have been inserted, the main procedure calls the InorderTraversal, PreorderTraversal, and PostorderTraversal procedures to display the tree in different orders.