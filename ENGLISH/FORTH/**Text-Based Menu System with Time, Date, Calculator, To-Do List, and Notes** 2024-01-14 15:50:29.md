```forth
: PRINT-MENU ( -- )
  BEGIN
    \ print main menu heading
    ." == [ GOOD MORNING ] =="
    ." 1. [ Current Time ]"
    ." 2. [ Current Date ]"
    ." 3. [ Calculator ]"
    ." 4. [ To Do List ]"
    ." 5. [ Notes ]"
    ." 6. [ Exit ]"
    ." Enter your choice (1-6): "
    ACCEPT
    DUP
  WHILE
    CASE
      OF 1
        : PRINT-CURRENT-TIME ( -- )
          TIME @
          4 SWAP -MOD 10 + SWAP 2DROP
          2DUP 60 /INT 2DROP
          2DUP 60 /INT 2DROP
          2DUP 10 + SWAP 10 * +
          100 + OVER 100 < IF 0 ELSE DROP THEN
          ." Current time: " 2DUP ." : " 2DUP ." : " 2DROP ." "
        ;
        PRINT-CURRENT-TIME
      OF 2
        : PRINT-CURRENT-DATE ( -- )
          DATE @
          2DROP 1000 /INT 3 PICK 2SWAP % 2DROP
          2DUP 12 /INT 3 PICK 2SWAP % 2DROP
          2DUP 31 /INT 3 PICK 2SWAP % 2DROP
          ." Current date: " 2DUP ." / " 2DUP ." / " 2DROP ." "
        ;
        PRINT-CURRENT-DATE
      OF 3
        : PRINT-CALCULATOR ( -- )
          BEGIN
            \ print calculator heading
            ." == [ CALCULATOR ] =="
            ." 1. [ Addition ]"
            ." 2. [ Subtraction ]"
            ." 3. [ Multiplication ]"
            ." 4. [ Division ]"
            ." 5. [ Exit ]"
            ." Enter your choice (1-5): "
            ACCEPT
            DUP
          WHILE
            CASE
              OF 1
                \ addition
                ." Enter first number: "
                ACCEPT
                ." Enter second number: "
                ACCEPT
                2SWAP + ." = " .
              OF 2
                \ subtraction
                ." Enter first number: "
                ACCEPT
                ." Enter second number: "
                ACCEPT
                2SWAP - ." = " .
              OF 3
                \ multiplication
                ." Enter first number: "
                ACCEPT
                ." Enter second number: "
                ACCEPT
                2SWAP * ." = " .
              OF 4
                \ division
                ." Enter first number: "
                ACCEPT
                ." Enter second number: "
                ACCEPT
                2SWAP / ." = " .
            ELSE
              \ exit calculator
            THEN
          REPEAT
        ;
        PRINT-CALCULATOR
      OF 4
        : PRINT-TODO-LIST ( -- )
          BEGIN
            \ print todo list heading
            ." == [ TODO LIST ] =="
            ." 1. [ Add Task ]"
            ." 2. [ View Tasks ]"
            ." 3. [ Delete Task ]"
            ." 4. [ Exit ]"
            ." Enter your choice (1-4): "
            ACCEPT
            DUP
          WHILE
            CASE
              OF 1
                \ add task
                ." Enter task description: "
                ACCEPT
                2DUP @ LENGTH TO-STRING
                2DUP @ . " added to list." CR
              OF 2
                \ view tasks
                ( "Task" "Description" ) 2DUP @ . CR
                BEGIN
                  2DUP LENGTH DO
                    DUP I @ 2DUP @ . " "
                  LOOP
                  2DROP CR
                REPEAT
              OF 3
                \ delete task
                ." Enter task number to delete: "
                ACCEPT
                2DUP @ LENGTH TO-STRING
                2DUP @ . " deleted from list." CR
              OF 4
                \ exit todo list
            ELSE
              \ exit todo list
            THEN
          REPEAT
        ;
        PRINT-TODO-LIST
      OF 5
        : PRINT-NOTES ( -- )
          BEGIN
            \ print notes heading
            ." == [ NOTES ] =="
            ." 1. [ Add Note ]"
            ." 2. [ View Notes ]"
            ." 3. [ Delete Note ]"
            ." 4. [ Exit ]"
            ." Enter your choice (1-4): "
            ACCEPT
            DUP
          WHILE
            CASE
              OF 1
                \ add note
                ." Enter note title: "
                ACCEPT
                2DUP @ LENGTH TO-STRING
                2DUP @ . " added to list." CR
              OF 2
                \ view notes
                ( "Note" "Title" ) 2DUP @ . CR
                BEGIN
                  2DUP LENGTH DO
                    DUP I @ 2DUP @ . " "
                  LOOP
                  2DROP CR
                REPEAT
              OF 3
                \ delete note
                ." Enter note number to delete: "
                ACCEPT
                2DUP @ LENGTH TO-STRING
                2DUP @ . " deleted from list." CR
              OF 4
                \ exit notes
            ELSE
              \ exit notes
            THEN
          REPEAT
        ;
        PRINT-NOTES
      OF 6
        EXIT
    ELSE
      ." Invalid choice. Please enter a number between 1 and 6." CR
    THEN
  REPEAT
  DROP
;

: GOOD-MORNING ( -- )
  BEGIN
    PRINT-MENU
    2DROP
  WHILE
    2DROP
  REPEAT
;

GOOD-MORNING
```

**Explanation:**

This Forth code is a text-based menu system that allows the user to perform various tasks, such as checking the current time and date, using a calculator, managing a to-do list, and taking notes.

The code begins with the definition of a function called `PRINT-MENU`, which displays the main menu options and prompts the user to enter their choice.

The `BEGIN` and `WHILE` keywords are used to create a loop that continuously displays the menu and waits for user input.

When the user enters a valid choice, the code branches to the appropriate function based on the user's input. These functions are defined using the `: ` and `;` keywords.

For example, if the user selects option 1, the `PRINT-CURRENT-TIME` function is called, which prints the current time in a human-readable format.

If the user selects option 2, the `PRINT-CURRENT-DATE` function is called, which prints the current date in a human-readable format.

The calculator is implemented using the `PRINT-CALCULATOR` function, which displays a submenu with options for addition, subtraction, multiplication, and division. The user can enter two numbers and select the desired operation, and the result is printed.

The to-do list is implemented using the `PRINT-TODO-LIST` function, which allows the user to add, view, and delete tasks.

The notes feature is implemented using the `PRINT-NOTES` function, which allows the user to add, view, and delete notes.

Finally, the `GOOD-MORNING` function is defined, which calls the `PRINT-MENU` function and loops until the user exits the program by selecting option 6.

Overall, this code demonstrates the use of various Forth features, including functions, loops, branching, and input/output operations.