```modula-2
MODULE StringSearch;
FROM Terminal IMPORT Write, WriteLn, Read, ReadLn;
FROM Strings IMPORT Substring, StringLength;
FROM CharConversion IMPORT ToUpperCase;

PROCEDURE Search(s: ARRAY OF CHAR; p: ARRAY OF CHAR;
                   i: CARDINAL; j: CARDINAL; n: CARDINAL;
                   m: CARDINAL; pos: OUT CARDINAL);

VAR
    k: CARDINAL;
    c: CHAR;

BEGIN
    k := 0;
    pos := 0;
    REPEAT
        IF i + k >= n THEN
            pos := 0;
            RETURN
        ELSIF ToUpperCase(s[i + k]) = ToUpperCase(p[j + k]) THEN
            INC(k);
            IF j + k = m THEN
                pos := i;
                RETURN
            END
        ELSE
            INC(i);
            Dec(pos, k);
            k := 0
        END
    UNTIL FALSE
END Search;

VAR
    s: ARRAY OF CHAR := "The quick brown fox jumps over the lazy dog";
    p: ARRAY OF CHAR := "jumps";
    n: CARDINAL := StringLength(s);
    m: CARDINAL := StringLength(p);
    i: CARDINAL := 1;
    j: CARDINAL := 1;
    pos: CARDINAL;

BEGIN
    REPEAT
        Search(s, p, i, j, n, m, pos);
        IF pos > 0 THEN
            WriteLn("Found at position ", pos);
            INC(i, pos);
            INC(j, pos)
        ELSE
            WriteLn("Not found")
        END
    UNTIL pos = 0
END StringSearch.
```

This code performs a case-insensitive search for a substring in a string. It uses a loop to iterate through the string and a nested loop to compare the substring to the string. If the substring is found, the position of the first character of the substring in the string is printed. Otherwise, a message is printed indicating that the substring was not found.

Here is a breakdown of the code:

* The `Search` procedure takes two arrays of characters (strings), two indices, and two lengths as input parameters. It also takes an output parameter, `pos`, which will be set to the position of the first character of the substring in the string if the substring is found.
* The `Search` procedure uses a loop to iterate through the string, starting at the index `i`. It also uses a nested loop to compare the substring to the string, starting at the index `j`.
* If the characters at the current indices in the string and the substring are equal, the loop continues to the next character.
* If the characters at the current indices are not equal, the loop moves to the next character in the string and decrements the `pos` variable by the number of characters that have been compared so far.
* If the `pos` variable becomes negative, the loop terminates and the `pos` variable is set to 0.
* If the loop reaches the end of the substring without finding a mismatch, the `pos` variable is set to the index of the first character of the substring in the string and the loop terminates.
* The `StringSearch` module uses a loop to repeatedly call the `Search` procedure, starting at different positions in the string.
* If the substring is found, the position of the first character of the substring in the string is printed. Otherwise, a message is printed indicating that the substring was not found.

This code demonstrates how to use loops and nested loops to search for a substring in a string in a case-insensitive manner.