```modula-2
MODULE System;
FROM Terminal IMPORT WriteString, WriteLn;

TYPE
  CharSet = ARRAY [Char] OF BOOLEAN;

PROCEDURE FillSet(VAR Set : CharSet; First, Last : CHAR);
VAR
  C : CHAR;
BEGIN
  FOR C := First TO Last DO
    Set[ORD(C)] := TRUE
  END;
  Set[ORD(' ')] := TRUE
END FillSet;

PROCEDURE RemoveChars(VAR Str, Set : CharSet);
VAR
  Pos, NewPos : INTEGER;
BEGIN
  Pos := 1;
  NewPos := 1;
  WHILE Pos <= LENGTH(Str) DO
    IF Set[ORD(Str[Pos])] THEN
      INC(Pos)
    ELSE BEGIN
      Str[NewPos] := Str[Pos];
      INC(NewPos);
      INC(Pos)
    END
  END;
  Str[NewPos] := '\0'
END RemoveChars;

PROCEDURE Concatenate(VAR Result, Str1, Str2 : STRING);
BEGIN
  Result := Str1 & Str2
END Concatenate;

PROCEDURE PrintStr(Str : STRING);
BEGIN
  WriteString(Str);
  WriteLn
END PrintStr;

VAR
  Str, Copy : STRING;
BEGIN
  FillSet(Set, 'A', 'Z');
  FillSet(Set, 'a', 'z');
  FillSet(Set, '0', '9');

  Str := "This is a test string with different kinds of characters";
  Copy := Str;

  PrintStr(Str);
  RemoveChars(Str, Set);
  PrintStr(Str);

  Concatenate(Str, " and this is the original string again", Copy);
  PrintStr(Str)
END System.
```

This code performs various string manipulation operations, including removing characters from a string, concatenating two strings, and printing the results. Here's how it works:

1. `FillSet` Procedure:
   - Initializes a character set (`Set`) by marking all characters from `First` to `Last` as `TRUE`.
   - Additionally, it marks the space character (' ') as `TRUE`.

2. `RemoveChars` Procedure:
   - Iterates through the input string (`Str`) character by character.
   - If a character is present in the `Set`, it's skipped.
   - If a character is not in the `Set`, it's copied to a new position in the string.
   - The resulting string contains only the characters that are not in the `Set`.

3. `Concatenate` Procedure:
   - Simply joins two strings (`Str1` and `Str2`) together and stores the result in `Result`.

4. `PrintStr` Procedure:
   - Prints the given string (`Str`) followed by a newline.

5. Main Program:
   - Creates a character set (`Set`) containing all uppercase letters, lowercase letters, and digits.
   - Initializes two strings, `Str` and `Copy`.
   - Calls `FillSet` to mark the characters in the set as `TRUE`.
   - Calls `PrintStr` to display the original string.
   - Calls `RemoveChars` to remove the characters in the set from `Str`.
   - Calls `PrintStr` again to display the modified string.
   - Calls `Concatenate` to join the modified string with the original string and store the result in `Str`.
   - Calls `PrintStr` one last time to display the final string, which includes both the modified and original versions.

This code demonstrates several string manipulation techniques and showcases how procedures can be used to perform specific tasks, making the code modular and reusable.