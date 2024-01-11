```modula-2
MODULE Modula2Program;
FROM Kernel IMPORT WriteString, WriteLn, WriteCard, WriteBool;
FROM Terminal IMPORT GetLine;
FROM Strings IMPORT String, Length, GetChar, SetChar;

TYPE CharArray = ARRAY 1..80 OF CHAR;

PROCEDURE PrintLine(s: String);
VAR i: CARDINAL;
BEGIN
  FOR i := 1 TO Length(s) DO
    WriteChar(GetChar(s, i))
  END;
  WriteLn
END PrintLine;

PROCEDURE PrintString(s: String);
VAR i: CARDINAL;
BEGIN
  FOR i := 1 TO Length(s) DO
    WriteChar(GetChar(s, i))
  END
END PrintString;

PROCEDURE PrintCard(n: CARDINAL);
BEGIN
  WriteCard(n)
END PrintCard;

PROCEDURE PrintBool(b: BOOLEAN);
BEGIN
  WriteBool(b)
END PrintBool;

VAR s: CharArray;
VAR i: CARDINAL;

BEGIN
  PrintString("Enter a string: ");
  GetLine(s);
  PrintLn;
  PrintString("The string you entered is: ");
  PrintLine(s);
  PrintLn;
  PrintString("The length of the string is: ");
  PrintCard(Length(s));
  PrintLn;
  PrintString("The first character of the string is: ");
  PrintChar(GetChar(s, 1));
  PrintLn;
  PrintString("The last character of the string is: ");
  PrintChar(GetChar(s, Length(s)));
  PrintLn;
  PrintString("Is the string empty? ");
  PrintBool(Length(s) = 0);
  PrintLn;
  PrintString("Enter a new character to replace the first character: ");
  GetLine(s);
  SetChar(s, 1, GetChar(s, 1));
  PrintLn;
  PrintString("The new string is: ");
  PrintLine(s)
END Modula2Program.
```

This Modula-2 program is a simple text editor that allows the user to enter a string, modify it, and display it. The program uses the following modules:

* Kernel: This module provides basic input/output functions, such as WriteString, WriteLn, WriteCard, and WriteBool.
* Terminal: This module provides functions for getting input from the user, such as GetLine.
* Strings: This module provides functions for working with strings, such as String, Length, GetChar, and SetChar.

The program first prompts the user to enter a string. The user's input is stored in the variable s. The program then uses the PrintLine procedure to display the string to the user.

Next, the program uses the PrintCard procedure to display the length of the string. The program then uses the GetChar procedure to get the first and last characters of the string. The program then uses the PrintBool procedure to display whether or not the string is empty.

The program then prompts the user to enter a new character to replace the first character of the string. The user's input is stored in the variable s. The program then uses the SetChar procedure to replace the first character of the string with the new character.

Finally, the program uses the PrintLine procedure to display the new string to the user.