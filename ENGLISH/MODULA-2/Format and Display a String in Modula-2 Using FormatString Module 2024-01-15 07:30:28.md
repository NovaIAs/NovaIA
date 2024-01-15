```modula-2
MODULE ComplexModula2Code;
FROM FormatString IMPORT FormatString, Format;

PROCEDURE Example(data: ARRAY OF CHAR);
VAR
    formatted: ARRAY[1..200] OF CHAR;
    i: INTEGER;
BEGIN
    i := 0;
    WHILE data[i] /= #0 DO
        INC(i);
    END;
    FormatString(formatted, 's32', [i]);
    Format(formatted, data);
    WriteLn(formatted);
END Example;

BEGIN
    Example([#'H', #'e', #'l', #'l', #'o', #' ', #'W', #'o', #'r', #'l', #'d', #'!']);
END ComplexModula2Code.
```

This code demonstrates the use of the FormatString and Format procedures from the FormatString module to format and display a string of characters. Here's a breakdown of the code:

1. Import Necessary Module:
   ```modula-2
   FROM FormatString IMPORT FormatString, Format;
   ```
   This line imports the FormatString module, which provides functions for formatting and displaying strings.

2. Define the Example Procedure:
   ```modula-2
   PROCEDURE Example(data: ARRAY OF CHAR);
   ```
   The Example procedure is defined to take an array of characters (a string) as its parameter.

3. Declare Local Variables:
   ```modula-2
   VAR
       formatted: ARRAY[1..200] OF CHAR;
       i: INTEGER;
   ```
   - `formatted`: An array of characters used to store the formatted string.
   - `i`: An integer variable used for iteration.

4. Calculate the Length of the Input String:
   ```modula-2
   i := 0;
   WHILE data[i] /= #0 DO
       INC(i);
   END;
   ```
   This loop iterates through the input string (`data`) until it encounters the null character (#0), which marks the end of the string. The loop calculates the length of the string by incrementing `i` for each character.

5. Format the String:
   ```modula-2
   FormatString(formatted, 's32', [i]);
   ```
   The FormatString procedure is used to format the string. It takes three parameters:
   - `formatted`: The array to store the formatted string.
   - `'s32'`: A format string specifying that the string should be formatted as a signed 32-bit integer.
   - `[i]`: An array containing the value to be formatted, in this case, the length of the input string.

6. Display the Formatted String:
   ```modula-2
   Format(formatted, data);
   ```
   The Format procedure is used to display the formatted string. It takes two parameters:
   - `formatted`: The array containing the formatted string.
   - `data`: The array containing the original string.

7. Write the Formatted String to the Console:
   ```modula-2
   WriteLn(formatted);
   ```
   The formatted string is written to the console using the WriteLn procedure.

8. Call the Example Procedure:
   ```modula-2
   Example([#'H', #'e', #'l', #'l', #'o', #' ', #'W', #'o', #'r', #'l', #'d', #'!']);
   ```
   The Example procedure is called with a string literal as an argument. This string is formatted and displayed using the FormatString and Format procedures.

When you run this program, it will display the formatted string "12 Hello World!". The length of the string, which is 12 characters, is formatted as a signed 32-bit integer and displayed before the string itself.