```modula-2
MODULE Example;
FROM Terminal IMPORT WriteString, WriteLn;

PROCEDURE main IS
    VAR i, j, k: INTEGER;
    VAR str: ARRAY 20 OF CHAR;

BEGIN
    i := 0;
    j := 1;
    k := 2;

    str := "Hello, world!";

    WriteString(str);
    WriteLn;

    FOR i := 1 TO 10 DO
        WriteString("The value of i is: ");
        WriteInt(i, 0);
        WriteLn
    END;

    WHILE j <= 10 DO
        WriteString("The value of j is: ");
        WriteInt(j, 0);
        WriteLn;
        j := j + 1
    END;

    REPEAT
        WriteString("The value of k is: ");
        WriteInt(k, 0);
        WriteLn;
        k := k + 1
    UNTIL k > 10

    WriteString("Press any key to continue...");
    ReadChar
END main.
```

This code is a simple example of a Modula-2 program. It prints the string "Hello, world!" to the console, then prints the values of the variables `i`, `j`, and `k`. It also demonstrates the use of the `FOR`, `WHILE`, and `REPEAT` loops.

Here is a more detailed explanation of the code:

* The `main` procedure is the entry point for the program.

* The `VAR` statements declare the variables that will be used in the program.

* The `BEGIN` and `END` statements mark the beginning and end of the program.

* The `WriteString` and `WriteLn` procedures are used to print text to the console.

* The `WriteInt` procedure is used to print an integer value to the console.

* The `FOR` loop iterates over a range of values, in this case from 1 to 10.

* The `WHILE` loop iterates while a condition is true, in this case while `j` is less than or equal to 10.

* The `REPEAT` loop iterates until a condition is true, in this case until `k` is greater than 10.

* The `Press any key to continue...` line prompts the user to press a key before continuing with the program.

* The `ReadChar` procedure waits for the user to press a key.