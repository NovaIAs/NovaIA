```modula-2

MODULE Strings;

FROM Terminal IMPORT WriteChar, WriteString, ReadChar, ReadString, WriteLn;

PROCEDURE stringLength(STRING: ARRAY OF CHAR): CARDINAL;
  VAR length: CARDINAL := 0;
BEGIN
  WHILE STRING[length] /= NUL DO
    INC(length)
  END;
  RETURN length;
END stringLength;

PROCEDURE stringCompare(STRING1, STRING2: ARRAY OF CHAR): BOOLEAN;
BEGIN
  VAR index: CARDINAL := 0;
  WHILE STRING1[index] /= NUL AND STRING2[index] /= NUL DO
    IF STRING1[index] /= STRING2[index] THEN
      RETURN FALSE
    END
  END;
  RETURN STRING1[index] = STRING2[index];
END stringCompare;

PROCEDURE stringConcatenate(STRING1, STRING2: ARRAY OF CHAR; RESULT: ARRAY OF CHAR);
BEGIN
  VAR index1, index2: CARDINAL := 0;
  WHILE STRING1[index1] /= NUL DO
    RESULT[index1] := STRING1[index1];
    INC(index1)
  END;
  WHILE STRING2[index2] /= NUL DO
    RESULT[index1 + index2] := STRING2[index2];
    INC(index2)
  END;
  RESULT[index1 + index2] := NUL
END stringConcatenate;

PROCEDURE stringCopy(SOURCE, DESTINATION: ARRAY OF CHAR);
BEGIN
  VAR index: CARDINAL := 0;
  WHILE SOURCE[index] /= NUL DO
    DESTINATION[index] := SOURCE[index];
    INC(index)
  END;
  DESTINATION[index] := NUL
END stringCopy;

PROCEDURE stringSearch(STRING: ARRAY OF CHAR; CHARACTER: CHAR): CARDINAL;
BEGIN
  VAR index: CARDINAL := 0;
  WHILE STRING[index] /= NUL DO
    IF STRING[index] = CHARACTER THEN
      RETURN index
    END;
    INC(index)
  END;
  RETURN -1
END stringSearch;

PROCEDURE stringReplace(STRING: ARRAY OF CHAR; OLD_CHARACTER, NEW_CHARACTER: CHAR);
BEGIN
  VAR index: CARDINAL := 0;
  WHILE STRING[index] /= NUL DO
    IF STRING[index] = OLD_CHARACTER THEN
      STRING[index] := NEW_CHARACTER
    END;
    INC(index)
  END
END stringReplace;

PROCEDURE stringToUpper(STRING: ARRAY OF CHAR);
BEGIN
  VAR index: CARDINAL := 0;
  WHILE STRING[index] /= NUL DO
    IF STRING[index] >= 'a' AND STRING[index] <= 'z' THEN
      STRING[index] := STRING[index] - ('a' - 'A')
    END;
    INC(index)
  END
END stringToUpper;

PROCEDURE stringToLower(STRING: ARRAY OF CHAR);
BEGIN
  VAR index: CARDINAL := 0;
  WHILE STRING[index] /= NUL DO
    IF STRING[index] >= 'A' AND STRING[index] <= 'Z' THEN
      STRING[index] := STRING[index] + ('a' - 'A')
    END;
    INC(index)
  END
END stringToLower;

PROCEDURE stringTrim(STRING: ARRAY OF CHAR);
BEGIN
  VAR start, end: CARDINAL;

  start := 0;
  WHILE STRING[start] = ' ' DO
    INC(start)
  END;

  end := stringLength(STRING) - 1;
  WHILE STRING[end] = ' ' DO
    DEC(end)
  END;

  stringCopy(STRING + start, STRING);
  STRING[end + 1] := NUL
END stringTrim;

PROCEDURE stringSplit(STRING: ARRAY OF CHAR; DELIMITER: CHAR; RESULT: ARRAY OF ARRAY OF CHAR): CARDINAL;
BEGIN
  VAR parts, index, startIndex: CARDINAL;
  parts := 0;

  startIndex := 0;
  FOR index := 0 TO stringLength(STRING) - 1 DO
    IF STRING[index] = DELIMITER OR STRING[index] = NUL THEN
      stringCopy(STRING + startIndex, RESULT[parts]);
      INC(parts);
      startIndex := index + 1
    END
  END;

  RETURN parts
END stringSplit;

PROCEDURE testStrings;
BEGIN
  VAR str1, str2: ARRAY[0..15] OF CHAR;
  VAR result: ARRAY[0..31] OF CHAR;

  WriteString("Enter a string: ");
  ReadString(str1);

  WriteString("Enter another string: ");
  ReadString(str2);

  WriteString("The length of the first string is: ");
  WriteCard(stringLength(str1));
  WriteLn;

  IF stringCompare(str1, str2) THEN
    WriteString("The two strings are equal.")
  ELSE
    WriteString("The two strings are not equal.")
  END;
  WriteLn;

  WriteString("The concatenated string is: ");
  stringConcatenate(str1, str2, result);
  WriteString(result);
  WriteLn;

  WriteString("The first string copied to the second string is: ");
  stringCopy(str1, str2);
  WriteString(str2);
  WriteLn;

  WriteString("The index of the character 'a' in the first string is: ");
  WriteCard(stringSearch(str1, 'a'));
  WriteLn;

  WriteString("The first string with all 'a' characters replaced with 'b' is: ");
  stringReplace(str1, 'a', 'b');
  WriteString(str1);
  WriteLn;

  WriteString("The first string in uppercase is: ");
  stringToUpper(str1);
  WriteString(str1);
  WriteLn;

  WriteString("The first string in lowercase is: ");
  stringToLower(str1);
  WriteString(str1);
  WriteLn;

  WriteString("The first string with leading and trailing spaces trimmed is: ");
  stringTrim(str1);
  WriteString(str1);
  WriteLn;

  WriteString("The first string split by space is: ");
  VAR parts: CARDINAL;
  parts := stringSplit(str1, ' ', result);
  FOR index := 0 TO parts - 1 DO
    WriteString(result[index]);
    WriteString(" ")
  END;
  WriteLn
END testStrings;

BEGIN
  testStrings
END Strings.

```