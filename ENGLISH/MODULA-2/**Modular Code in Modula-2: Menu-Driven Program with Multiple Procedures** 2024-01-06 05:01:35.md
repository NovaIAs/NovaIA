```modula-2
MODULE ComplexCode;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

PROCEDURE PrintAlphabet();
VAR
  Letter: CHAR;
BEGIN
  FOR Letter := 'A' TO 'Z' DO
    WriteString(Letter);
  WriteString(NewLine);
END PrintAlphabet;

PROCEDURE PrintNumbers(Start, End: CARDINAL);
VAR
  Number: CARDINAL;
BEGIN
  FOR Number := Start TO End DO
    WriteCard(Number, 0);
  WriteString(NewLine);
END PrintNumbers;

PROCEDURE PrintPyramid(Height: CARDINAL);
VAR
  Level, Number: CARDINAL;
BEGIN
  FOR Level := 1 TO Height DO
  BEGIN
    FOR Number := 1 TO Level DO
      WriteCard(Number, 0);
    WriteString(NewLine);
  END;
END PrintPyramid;

PROCEDURE PrintTable(Rows, Columns: CARDINAL);
VAR
  Row, Column: CARDINAL;
BEGIN
  FOR Row := 1 TO Rows DO
  BEGIN
    FOR Column := 1 TO Columns DO
      WriteCard(Row * Column, 0);
    WriteString(NewLine);
  END;
END PrintTable;

PROCEDURE Main();
VAR
  Choice: CHAR;
BEGIN
  REPEAT
    WriteString("1. Print Alphabet");
    WriteString(NewLine);
    WriteString("2. Print Numbers");
    WriteString(NewLine);
    WriteString("3. Print Pyramid");
    WriteString(NewLine);
    WriteString("4. Print Table");
    WriteString(NewLine);
    WriteString("5. Exit");
    WriteString(NewLine);
    WriteString("Enter your choice: ");
    ReadChar(Choice);
    WriteString(NewLine);

    CASE Choice OF
      '1': PrintAlphabet();
      '2':
        VAR Start, End: CARDINAL;
        WriteString("Enter start number: ");
        ReadCard(Start);
        WriteString("Enter end number: ");
        ReadCard(End);
        PrintNumbers(Start, End);
      '3':
        VAR Height: CARDINAL;
        WriteString("Enter pyramid height: ");
        ReadCard(Height);
        PrintPyramid(Height);
      '4':
        VAR Rows, Columns: CARDINAL;
        WriteString("Enter number of rows: ");
        ReadCard(Rows);
        WriteString("Enter number of columns: ");
        ReadCard(Columns);
        PrintTable(Rows, Columns);
      '5': EXIT;
      ELSE: WriteString("Invalid choice!");
    END;

    WriteString(NewLine);
    WriteString("Press any key to continue...");
    ReadChar(Choice);
    WriteString(NewLine);
  UNTIL Choice = '5';
END Main;

BEGIN
  Main();
END ComplexCode.
```

**Explanation:**

This Modula-2 code is a complex and differentiated program that includes various procedures for performing different tasks. Here's a detailed explanation of the code:

1. **PrintAlphabet Procedure:**

   - This procedure prints the entire alphabet from 'A' to 'Z' in a single line.

2. **PrintNumbers Procedure:**

   - This procedure takes two parameters, `Start` and `End`, which specify the range of numbers to be printed.
   - It prints the numbers from `Start` to `End` in a single line, separated by spaces.

3. **PrintPyramid Procedure:**

   - This procedure takes one parameter, `Height`, which specifies the height of the pyramid to be printed.
   - It prints a pyramid of numbers, with each row containing one more number than the previous row.

4. **PrintTable Procedure:**

   - This procedure takes two parameters, `Rows` and `Columns`, which specify the dimensions of the table to be printed.
   - It prints a table of numbers, with the rows and columns labeled accordingly.

5. **Main Procedure:**

   - This is the main procedure of the program.
   - It presents a menu of options to the user, allowing them to choose which task they want to perform.
   - Depending on the user's choice, it calls the appropriate procedure to perform the task.

6. **Main Loop:**

   - The program enters a loop where it repeatedly displays the menu, gets the user's choice, and performs the selected task.
   - The loop continues until the user chooses to exit the program.

This code demonstrates various programming concepts such as procedures, parameters, loops, and user interaction. It is a good example of how to structure a program into modular components and how to handle user input and choices.