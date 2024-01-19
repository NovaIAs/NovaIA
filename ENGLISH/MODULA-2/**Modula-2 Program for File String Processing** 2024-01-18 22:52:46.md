```modula-2
MODULE SampleModula2;
FROM FormatString IMPORT Format;
FROM Strings IMPORT Length;

TYPE
  StringArray = ARRAY OF STRING;
  StringProc = PROCEDURE (STRING) : BOOLEAN;
  FileHandle = REF FILE;

PROCEDURE ProcessStrings(file: FileHandle; proc: StringProc);
VAR
  str: ARRAY [1..80] OF CHAR;
  len: CARDINAL;
BEGIN
  LOOP
    len := ReadLine(file, str);
    IF (len = 0) OR proc(str) THEN LEAVE;
  END;
END ProcessStrings;

PROCEDURE DisplayMessage(msg: STRING);
BEGIN
  WriteCard(Length(msg), 0);
  WriteString(msg, 0);
  WriteLn;
END DisplayMessage;

PROCEDURE StringFilter(s: STRING) : BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  i := Length(s);
  WHILE i > 0 DO
    IF (s[i] = ' ') THEN DEC(i);
    ELSE LEAVE
  END;
  RETURN (i = 0);
END StringFilter;

PROCEDURE Main;
VAR
  file: FileHandle;
BEGIN
  file := Open("myfile.txt", Read);
  IF file = NIL THEN
    DisplayMessage("Could not open file");
  ELSE
    DisplayMessage("Processing file:");
    ProcessStrings(file, StringFilter);
    Close(file);
  END;
END Main.
```

Explanation:

1. MODULE SampleModula2: Defines the beginning of the Modula-2 module named SampleModula2.

2. FROM FormatString IMPORT Format: Imports the Format procedure from the FormatString module, which is used to format output.

3. FROM Strings IMPORT Length: Imports the Length function from the Strings module, which returns the length of a string.

4. TYPE Declarations:
   - StringArray: An array of strings.
   - StringProc: A procedure that takes a string as input and returns a Boolean value.
   - FileHandle: A reference to a file.

5. PROCEDURE ProcessStrings:
   - This procedure reads lines from a file and processes each line using a string processing procedure (in this case, StringFilter).
   - It reads each line into the str array, checks its length, and calls the procedure passed as an argument (proc) with the string.

6. PROCEDURE DisplayMessage:
   - This procedure takes a string as an argument and displays it along with its length.

7. PROCEDURE StringFilter:
   - This procedure takes a string as an argument and returns true if the string contains only spaces or is empty.
   - It iterates through the string characters from the end and returns true if all characters are spaces.

8. PROCEDURE Main:
   - This is the entry point of the program.
   - It opens a text file named "myfile.txt" for reading.
   - If the file cannot be opened, an error message is displayed.
   - Otherwise, it calls ProcessStrings to process the file's contents using StringFilter.

9. The program reads lines from the file, filters out empty or whitespace-only lines, and displays processed lines along with their lengths.