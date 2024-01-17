```modula-2

MODULE DataStructureOperations;
FROM Terminal IMPORT WriteString, WriteLn, WriteCard;
FROM TerminalUtilities IMPORT GetInt;
FROM StruckOut IMPORT ArraySize;

TYPE
  TypeSet = SET OF CHAR;
  TypeArray = ARRAY [0..99] OF INTEGER;
  TypeLink = RECORD
               value : CARDINAL;
               next  : CARDINAL;
              END;

PROCEDURE PrintArray(iArray : TypeArray);
VAR
  i : CARDINAL;
BEGIN
  WriteString('0');
  FOR i := 1 TO ArraySize(iArray) DO
    WriteCard(iArray[i]);
  END;
  WriteLn;
END PrintArray;

PROCEDURE Exchange(VAR i0, i1 : INTEGER);
BEGIN
  VAR iSwap : INTEGER;
  iSwap := i0;
  i0 := i1;
  i1 := iSwap;
END Exchange;

PROCEDURE BubbleSort(VAR iArray : TypeArray);
VAR
  iPass, i, j : CARDINAL;
BEGIN
  FOR iPass := ArraySize(iArray) TO 1 BY -1 DO
    FOR i := 1 TO iPass - 1 DO
      IF iArray[i] > iArray[i + 1] THEN
        Exchange(iArray[i], iArray[i + 1]);
      END;
    END;
  END;
END BubbleSort;

PROCEDURE PrintSet(iSet : TypeSet);
VAR
  i : CARDINAL;
BEGIN
  WriteString('{');
  FOR i := 0 TO 25 DO
    IF iSet[CHR(i + 65)] THEN
      WriteCard(i + 65);
    END;
  END;
  WriteString('}');
  WriteLn;
END PrintSet;

PROCEDURE Which(iSet : TypeSet) : CHAR;
VAR
  i : CARDINAL;
BEGIN
  FOR i := 0 TO 25 DO
    IF iSet[CHR(i + 65)] THEN
      RETURN CHR(i + 65);
    END;
  END;
  RETURN '\0';
END Which;

PROCEDURE Remove(VAR iSet : TypeSet; iChar : CHAR);
BEGIN
  IF iSet[iChar] THEN
    iSet := iSet - [iChar];
  END;
END Remove;

PROCEDURE Add(VAR iSet : TypeSet; iChar : CHAR);
BEGIN
  iSet := iSet + [iChar];
END Add;

PROCEDURE Overlap(iSet0, iSet1 : TypeSet) : BOOLEAN;
VAR
  iChar : CHAR;
BEGIN
  FOR iChar := 'A' TO 'Z' DO
    IF iSet0[iChar] AND iSet1[iChar] THEN
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END Overlap;

PROCEDURE BuildSet(iText : ARRAY OF CHAR) : TypeSet;
VAR
  i : CARDINAL;
BEGIN
  RESULT := [];
  FOR i := 0 TO ArraySize(iText) - 1 DO
    IF iText[i] IN 'A'..'Z' THEN
      Add(RESULT, iText[i]);
    END;
  END;
END BuildSet;

PROCEDURE RemoveLinkedList(VAR iFirst : CARDINAL);
VAR
  iNext : CARDINAL;
BEGIN
  WHILE iFirst /= NIL DO
    iNext := iFirst^.next;
    Dispose(iFirst);
    iFirst := iNext;
  END;
END RemoveLinkedList;

PROCEDURE PrintLinkedList(iFirst : CARDINAL);
VAR
  iElement : CARDINAL;
BEGIN
  iElement := iFirst;
  WHILE iElement /= NIL DO
    WriteCard(iElement^.value);
    WriteCard(iElement^.next);
    iElement := iElement^.next;
  END;
  WriteLn;
END PrintLinkedList;

PROCEDURE InsertLinkedList(VAR iFirst : CARDINAL; iValue : CARDINAL);
VAR
  iNewElement : TypeLink;
BEGIN
  NEW(iNewElement);
  iNewElement^.value := iValue;
  iNewElement^.next  := iFirst;
  iFirst := iNewElement;
END InsertLinkedList;

PROCEDURE RemoveLinkedListElement(VAR iFirst : CARDINAL; iValue : CARDINAL) : BOOLEAN;
VAR
  iPrevious, iLink : CARDINAL;
BEGIN
  iPrevious := NIL;
  iLink := iFirst;
  WHILE (iLink /= NIL) AND (iLink^.value /= iValue) DO
    iPrevious := iLink;
    iLink := iLink^.next;
  END;
  IF iLink /= NIL THEN
    IF iPrevious = NIL THEN
      iFirst := iLink^.next;
    ELSE
      iPrevious^.next := iLink^.next;
    END;
    Dispose(iLink);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END RemoveLinkedListElement;

PROCEDURE RemoveLinkedListNode(VAR iFirst : CARDINAL; iNode : CARDINAL) : BOOLEAN;
VAR
  iPrevious, iLink : CARDINAL;
BEGIN
  iPrevious := NIL;
  iLink := iFirst;
  WHILE (iLink /= NIL) AND (iLink /= iNode) DO
    iPrevious := iLink;
    iLink := iLink^.next;
  END;
  IF iLink /= NIL THEN
    IF iPrevious = NIL THEN
      iFirst := iLink^.next;
    ELSE
      iPrevious^.next := iLink^.next;
    END;
    Dispose(iLink);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END RemoveLinkedListNode;

PROCEDURE MergeLinkedLists(iFirst0, iFirst1 : CARDINAL) : CARDINAL;
VAR
  iElement0, iElement1, iResult : CARDINAL;
BEGIN
  iElement0 := iFirst0;
  iElement1 := iFirst1;
  iResult := NIL;
  WHILE (iElement0 /= NIL) AND (iElement1 /= NIL) DO
    IF iElement0^.value < iElement1^.value THEN
      InsertLinkedList(iResult, iElement0^.value);
      iElement0 := iElement0^.next;
    ELSE
      InsertLinkedList(iResult, iElement1^.value);
      iElement1 := iElement1^.next;
    END;
  END;
  WHILE iElement0 /= NIL DO
    InsertLinkedList(iResult, iElement0^.value);
    iElement0 := iElement0^.next;
  END;
  WHILE iElement1 /= NIL DO
    InsertLinkedList(iResult, iElement1^.value);
    iElement1 := iElement1^.next;
  END;
  RETURN iResult;
END MergeLinkedLists;

PROCEDURE SortLinkedList(VAR iFirst : CARDINAL);
VAR
  iLink, iNext, iList1, iList2 : CARDINAL;
BEGIN
  IF iFirst = NIL OR iFirst^.next = NIL THEN
    RETURN;
  END;
  iList1 := iFirst;
  iList2 := NIL;
  WHILE iList1 /= NIL DO
    iNext := iList1^.next;
    InsertLinkedList(iList2, iList1^.value);
    iList1 := iNext;
  END;
  RemoveLinkedList(iFirst);
  iList1 := iList2;
  WHILE iList1 /= NIL DO
    iList2 := iList1^.next;
    iLink := iList2;
    WHILE iLink /= NIL DO
      IF iList1^.value > iLink^.value THEN
        Exchange(iList1^.value, iLink^.value);
      END;
      iLink := iLink^.next;
    END;
    iList1 := iList2;
  END;
  iFirst := iList2;
END SortLinkedList;

PROCEDURE ReverseLinkedList(VAR iFirst : CARDINAL);
VAR
  iLink, iNext, iReversed : CARDINAL;
BEGIN
  iNext := NIL;
  WHILE iFirst /= NIL DO
    iLink := iFirst^.next;
    iFirst^.next := iNext;
    iNext := iFirst;
    iFirst := iLink;
  END;
  iReversed := iNext;
  iFirst := iReversed;
END ReverseLinkedList;

PROCEDURE CountLinkedListElements(iFirst : CARDINAL) : CARDINAL;
VAR
  iElement, iCount : CARDINAL;
BEGIN
  iElement := iFirst;
  iCount := 0;
  WHILE iElement /= NIL DO
    iCount := iCount + 1;
    iElement := iElement^.next;
  END;
  RETURN iCount;
END CountLinkedListElements;

PROCEDURE FreeUnusedLinkedListNodes(VAR iFirst : CARDINAL);
VAR
  iLink : CARDINAL;
BEGIN
  WHILE iFirst /= NIL DO
    iLink := iFirst^.next;
    Dispose(iFirst);
    iFirst := iLink;
  END;
END FreeUnusedLinkedListNodes;

PROCEDURE JavaStyleLinkedList(VAR iFirst : CARDINAL);
VAR
  iLink : CARDINAL;
BEGIN
  FreeUnusedLinkedListNodes(iFirst);
  iLink := iFirst;
  WHILE iLink /= NIL DO
    iLink := iLink^.next;
  END;
END JavaStyleLinkedList;

VAR IntArray : ARRAY[1..10] OF INTEGER;
    CharSet   : TypeSet;
    LinkedList : CARDINAL;

BEGIN

  FOR IntArray[1] := 1 TO 10 DO
    IntArray[IntArray[1]] := IntArray[1] + 1;
  END;
  BubbleSort(IntArray);
  FOR IntArray[1] := 1 TO 10 DO
    WriteCard(IntArray[IntArray[1]]);
  END;
  WriteLn;

  CharSet := BuildSet('DATAPLICATIONDEVELOPMENT');
  PrintLinkedList(LinkedList);
  PrintSet(CharSet);

  Add(CharSet, 'X');
  Remove(CharSet, 'D');
  IF Overlap(CharSet, BuildSet('BASIC')) THEN
    WriteString('OVERLAP');
  ELSE
    WriteString('NO OVERLAP');
  END;
  WriteLn;
  PrintSet(CharSet);

  InsertLinkedList(LinkedList, 12);
  InsertLinkedList(LinkedList, 23);
  InsertLinkedList(LinkedList, 47);
  InsertLinkedList(LinkedList, 15);
  InsertLinkedList(LinkedList, 37);
  PrintLinkedList(LinkedList);

  RemoveLinkedListElement(LinkedList, 37);
  RemoveLinkedListElement(LinkedList, 15);
  PrintLinkedList(LinkedList);

  RemoveLinkedListNode(LinkedList, LinkedList^.next);
  PrintLinkedList(LinkedList);

  LinkedList := MergeLinkedLists(LinkedList, LinkedList);
  SortLinkedList(LinkedList);
  PrintLinkedList(LinkedList);

  ReverseLinkedList(LinkedList);
  PrintLinkedList(LinkedList);

  WriteString(CountLinkedListElements(LinkedList).STRING);
  JavaStyleLinkedList(LinkedList);
  WriteLn;

END DataStructureSamples.

EXPLANATION:

- The code you provided is a complex and differentiated code in MODULA-2. It contains various procedures and functions that demonstrate different data structures and their operations.
- The code is divided into several parts:
  - **Sorting an Array**: This part of the code sorts an array of integers in ascending order using the Bubble Sort algorithm.
  - **Set Operations**: This part of the code demonstrates set operations such as building a set, adding and removing elements from a set, checking for set overlap, and printing the contents of a set.
  - **Linked List Operations**: This part of the code demonstrates various linked list operations such as inserting, deleting, and merging linked lists, sorting a linked list, reversing a linked list, counting the number of elements in a linked list, and freeing unused linked list nodes.
- The code uses procedures and functions to perform various tasks, making it easier to understand and maintain.
- The code also includes comments to explain the purpose of each part of the code, making it更容易理解和维护。
- The code demonstrates various data structures and their operations, making it a good example of how to use data structures in MODULA-2.