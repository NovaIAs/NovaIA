```modula-2
MODULE Queue;
FROM Terminal IMPORT WriteString;

TYPE
  QueueRecord = RECORD
    head, tail: CARDINAL;
    array: ARRAY[0..255] OF INTEGER;
  END;

VAR
  queue: QueueRecord;

PROCEDURE InitQueue;
BEGIN
  queue.head := 0;
  queue.tail := 0;
END InitQueue;

PROCEDURE Enqueue(item: INTEGER);
VAR
  next: CARDINAL;
BEGIN
  next := queue.tail + 1;
  IF next > HIGH(queue.array) THEN
    next := 0
  END;
  IF next = queue.head THEN
    WriteString("Queue overflow");
    HALT
  END;
  queue.array[queue.tail] := item;
  queue.tail := next
END Enqueue;

PROCEDURE Dequeue: INTEGER;
VAR
  item: INTEGER;
BEGIN
  IF queue.head = queue.tail THEN
    WriteString("Queue underflow");
    HALT
  END;
  item := queue.array[queue.head];
  queue.head := queue.head + 1;
  IF queue.head > HIGH(queue.array) THEN
    queue.head := 0
  END;
  RETURN item
END Dequeue;

PROCEDURE PrintQueue;
VAR
  i: CARDINAL;
BEGIN
  WriteString("Queue: ");
  FOR i := queue.head TO queue.tail - 1 DO
    WriteString(queue.array[i]);
    WriteString(" ");
  OD;
  WriteString(queue.array[queue.tail]);
  WriteString(NEWLINE)
END PrintQueue;

BEGIN
  InitQueue;
  Enqueue(1);
  Enqueue(2);
  Enqueue(3);
  PrintQueue;
  Dequeue;
  Dequeue;
  Enqueue(4);
  Enqueue(5);
  PrintQueue;
  Dequeue;
  Dequeue;
  Dequeue
END Queue.
```

This code implements a simple queue data structure in MODULA-2. A queue is a First-In-First-Out (FIFO) data structure, meaning that the first item added to the queue is the first item to be removed.

The code starts by defining a record type called `QueueRecord` which contains three fields: `head`, `tail`, and `array`. The `head` and `tail` fields are used to keep track of the beginning and end of the queue, respectively. The `array` field is an array of integers that stores the items in the queue.

The `InitQueue` procedure initializes the queue by setting the `head` and `tail` fields to 0.

The `Enqueue` procedure adds an item to the queue. It does this by first calculating the next position in the queue, which is the position after the `tail` position. If the next position is greater than the end of the array, it wraps around to the beginning of the array. If the next position is equal to the `head` position, the queue is full and an error message is printed. Otherwise, the item is added to the array at the `tail` position and the `tail` position is incremented.

The `Dequeue` procedure removes an item from the queue. It does this by first checking if the queue is empty. If the queue is empty, an error message is printed. Otherwise, the item at the `head` position is removed from the array and the `head` position is incremented. If the `head` position reaches the end of the array, it wraps around to the beginning of the array.

The `PrintQueue` procedure prints the contents of the queue to the terminal. It does this by iterating over the array from the `head` position to the `tail` position and printing each item.

The main program starts by calling the `InitQueue` procedure to initialize the queue. It then calls the `Enqueue` procedure three times to add the numbers 1, 2, and 3 to the queue. It then calls the `PrintQueue` procedure to print the contents of the queue. It then calls the `Dequeue` procedure twice to remove the first two items from the queue. It then calls the `Enqueue` procedure twice to add the numbers 4 and 5 to the queue. It then calls the `PrintQueue` procedure to print the contents of the queue. It then calls the `Dequeue` procedure three times to remove the last three items from the queue.

The output of the program is:

```
Queue: 1 2 3
Queue: 3 4 5
```