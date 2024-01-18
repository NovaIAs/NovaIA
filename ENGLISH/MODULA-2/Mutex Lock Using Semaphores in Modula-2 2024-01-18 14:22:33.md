```modula-2
MODULE Mutex;

FROM Terminal IMPORT WriteString;
FROM Shell IMPORT GETCH;

TYPE
  Semaphore = RECORD
    count: LONGINT;
    queue: POINTER TO SemaphoreNode;
  END;

TYPE
  SemaphoreNode = RECORD
    thread: POINTER TO Thread;
    next: POINTER TO SemaphoreNode;
  END;

PROCEDURE InitSemaphore(VAR s: Semaphore): BOOLEAN;
BEGIN
  s.count := 0;
  s.queue := NIL;
  RETURN TRUE
END InitSemaphore;

PROCEDURE WaitSemaphore(VAR s: Semaphore): BOOLEAN;
VAR
  waiting: BOOLEAN;
  node: POINTER TO SemaphoreNode;
BEGIN
  WHILE s.count < 1 DO
    waiting := TRUE;
    NEW(node);
    node^.thread := CallingThread;
    node^.next := s.queue;
    s.queue := node;
    SuspendThread(CallingThread);
  END;

  s.count := s.count - 1;
  RETURN waiting
END WaitSemaphore;

PROCEDURE SignalSemaphore(VAR s: Semaphore): BOOLEAN;
VAR
  node: POINTER TO SemaphoreNode;
BEGIN
  IF s.count = 0 THEN
    IF s.queue = NIL THEN
      s.count := s.count + 1;
      RETURN FALSE
    ELSE
      node := s.queue;
      s.queue := s.queue^.next;
      ResumeThread(node^.thread);
      RETURN TRUE
    END
  ELSE
    s.count := s.count + 1;
    RETURN FALSE
  END
END SignalSemaphore;

VAR
  mutex: Semaphore;

PROCEDURE AThread: PROCEDURE;
BEGIN
  WHILE TRUE DO
    WriteString("A");
    WaitSemaphore(mutex);
    WriteString("B");
    SignalSemaphore(mutex);
  END
END AThread;

PROCEDURE BThread: PROCEDURE;
BEGIN
  WHILE TRUE DO
    WaitSemaphore(mutex);
    WriteString("C");
    SignalSemaphore(mutex);
    WriteString("D");
  END
END BThread;

BEGIN
  InitSemaphore(mutex);
  NEW(AThread);
  NEW(BThread);
  GETCH;
  Exit
END Mutex.
```

This code implements a mutex (mutual exclusion) lock using semaphores in Modula-2. A mutex is a synchronization primitive that allows multiple threads to access a shared resource in a mutually exclusive manner, meaning that only one thread can access the resource at a time.

The code first defines the Semaphore and SemaphoreNode record types. The Semaphore record type contains a count field, which stores the number of threads that are currently waiting for the mutex, and a queue field, which stores a linked list of the threads that are waiting for the mutex. The SemaphoreNode record type contains a thread field, which stores a pointer to the thread that is waiting for the mutex, and a next field, which stores a pointer to the next node in the linked list.

The code then defines three procedures: InitSemaphore, WaitSemaphore, and SignalSemaphore. The InitSemaphore procedure initializes a semaphore to an empty state. The WaitSemaphore procedure waits for a semaphore to become available. If the semaphore is available, the procedure returns immediately. Otherwise, the procedure adds the calling thread to the queue of threads that are waiting for the semaphore and suspends the thread. The SignalSemaphore procedure signals a semaphore, which allows one of the threads that is waiting for the semaphore to resume execution.

The code then defines two thread procedures, AThread and BThread. The AThread procedure prints the letters "A" and "B" to the console, while the BThread procedure prints the letters "C" and "D" to the console. Both procedures use the mutex to ensure that only one thread can access the console at a time.

Finally, the code creates two instances of the AThread and BThread procedures and then waits for the user to press a key before exiting.