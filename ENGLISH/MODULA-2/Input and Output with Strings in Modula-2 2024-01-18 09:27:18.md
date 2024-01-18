```modula-2
MODULE LongAndComplexCode;
FROM Terminal IMPORT WriteString, WriteLn, WriteInt;
FROM Strings IMPORT SubString, Length, String;
FROM Codes IMPORT ReadChar, NewLine;

VAR
  s: String;
  i: CARDINAL;

PROCEDURE PrintString(s: String);
BEGIN
  WriteString(s);
END PrintString;

PROCEDURE PrintInt(i: CARDINAL);
BEGIN
  WriteInt(i, 0);
END PrintInt;

PROCEDURE PrintNewLine;
BEGIN
  WriteLn;
END PrintNewLine;

PROCEDURE ReadString(VAR s: String);
BEGIN
  i := 0;
  WHILE (i < Length(s)) AND (ReadChar() <> NewLine) DO
    s[i + 1] := ReadChar();
    i := i + 1;
  END;
  s[i + 1] := '\0';
END ReadString;

PROCEDURE Main;
BEGIN
  PrintString("Enter a string: ");
  ReadString(s);
  PrintNewLine;
  PrintString("The string you entered is: ");
  PrintString(s);
  PrintNewLine;
  PrintInt(Length(s));
  PrintString(" characters long.");
  PrintNewLine;
  PrintString("The first character is: ");
  PrintString(SubString(s, 1, 1));
  PrintNewLine;
  PrintString("The last character is: ");
  PrintString(SubString(s, Length(s), 1));
  PrintNewLine;
END Main.
```

Explanation:

This code is a simple program that prompts the user to enter a string, then prints the string, its length, its first character, and its last character. It uses several procedures and functions from the standard Modula-2 libraries, including `Terminal` for input and output, `Strings` for string manipulation, and `Codes` for character input.

The main procedure, `Main`, calls the `PrintString` procedure to prompt the user to enter a string. The `ReadString` procedure is then called to read the string from the console. The `PrintNewLine` procedure is used to print a newline character.

The `Length` function is used to get the length of the string, and the `SubString` function is used to extract the first and last characters of the string.

The output of the program will be something like this:

```
Enter a string: Hello, world!

The string you entered is: Hello, world!
13 characters long.
The first character is: H
The last character is: !
```