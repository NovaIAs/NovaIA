```modula-2
MODULE TextProcessing;
FROM FormatString IMPORT Format;
FROM Strings IMPORT StrTrim;
TYPE Text = ARRAY OF CHAR;
VAR TempText: Text;

PROCEDURE MakeLower(VAR Str: Text);
VAR I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(Str) DO
    IF Str[I] >= 'A' AND Str[I] <= 'Z' THEN
      Str[I] := Str[I] + ('a' - 'A');
    END;
  END;
END MakeLower;

PROCEDURE RemoveDuplicates(VAR Str: Text);
VAR I, J, K: CARDINAL;
BEGIN
  I := 0;
  WHILE I <= HIGH(Str) DO
    J := I + 1;
    WHILE J <= HIGH(Str) DO
      IF Str[I] = Str[J] THEN
        FOR K := J TO HIGH(Str) DO
          Str[K] := Str[K + 1];
        END;
        DEC(HIGH(Str));
      ELSE
        INC(J);
      END;
    END;
    INC(I);
  END;
END RemoveDuplicates;

PROCEDURE ReplaceString(VAR Str: Text; Old, New: Text);
VAR I, J, K: CARDINAL;
BEGIN
  WHILE (I := INDEX(Str, Old)) > 0 DO
    FOR J := HIGH(Str) TO I + HIGH(New) DO
      Str[J + HIGH(New) - HIGH(Old)] := Str[J];
    END;
    FOR K := 0 TO HIGH(New) DO
      Str[I + K] := New[K];
    END;
    INC(HIGH(Str), HIGH(New) - HIGH(Old));
  END;
END ReplaceString;

PROCEDURE TitleCase(VAR Str: Text);
VAR I: CARDINAL;
BEGIN
  MakeLower(Str);
  FOR I := 0 TO HIGH(Str) DO
    IF IsSpace(Str[I]) THEN
      Str[I + 1] := Str[I + 1] - ('a' - 'A');
    END;
  END;
  Str[0] := Str[0] - ('a' - 'A');
END TitleCase;

PROCEDURE FormatText(VAR Str: Text);
VAR Args: ARRAY[0..5] OF TEXT;
    I: CARDINAL;
BEGIN
  IF Str = '' THEN
    RETURN;
  END;
  StrTrim(Str);
  FOR I := 0 TO HIGH(Args) DO
    Args[I] := '';
  END;
  WHILE POS('%', Str) > 0 DO
    I := LENGTH(Str) - POS('%', Str);
    IF I > 5 THEN
      ERROR("Too many format specifiers", Str);
    END;
    INC(Args[I]);
  END;
  Format(Str, Args);
END FormatText;

PROCEDURE ConvertToMorseCode(VAR Str: Text);
VAR CharSet: ARRAY OF CHAR;
    MorseCode: ARRAY[0..255] OF TEXT;
    I: CARDINAL;
BEGIN
  CharSet := "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  MorseCode[ord('A')] := ".-";
  MorseCode[ord('B')] := "-...";
  MorseCode[ord('C')] := "-.-.";
  MorseCode[ord('D')] := "-..";
  MorseCode[ord('E')] := ".";
  MorseCode[ord('F')] := "..-.";
  MorseCode[ord('G')] := "--.";
  MorseCode[ord('H')] := "....";
  MorseCode[ord('I')] := "..";
  MorseCode[ord('J')] := ".---";
  MorseCode[ord('K')] := "-.-";
  MorseCode[ord('L')] := ".-..";
  MorseCode[ord('M')] := "--";
  MorseCode[ord('N')] := "-.";
  MorseCode[ord('O')] := "---";
  MorseCode[ord('P')] := ".--.";
  MorseCode[ord('Q')] := "--.-";
  MorseCode[ord('R')] := ".-.";
  MorseCode[ord('S')] := "...";
  MorseCode[ord('T')] := "-";
  MorseCode[ord('U')] := "..-";
  MorseCode[ord('V')] := "...-";
  MorseCode[ord('W')] := ".--";
  MorseCode[ord('X')] := "-..-";
  MorseCode[ord('Y')] := "-.--";
  MorseCode[ord('Z')] := "--..";
  MorseCode[ord('0')] := "-----";
  MorseCode[ord('1')] := ".----";
  MorseCode[ord('2')] := "..---";
  MorseCode[ord('3')] := "...--";
  MorseCode[ord('4')] := "....-";
  MorseCode[ord('5')] := ".....";
  MorseCode[ord('6')] := "-....";
  MorseCode[ord('7')] := "--...";
  MorseCode[ord('8')] := "---..";
  MorseCode[ord('9')] := "----.";
  StrTrim(Str);
  FOR I := 0 TO HIGH(Str) DO
    Str[I] := MorseCode[ord(Str[I])];
  END;
END ConvertToMorseCode;

PROCEDURE ReverseString(VAR Str: Text);
VAR I, J: CARDINAL;
    Temp: CHAR;
BEGIN
  I := 0;
  J := HIGH(Str);
  WHILE I < J DO
    Temp := Str[I];
    Str[I] := Str[J];
    Str[J] := Temp;
    INC(I);
    DEC(J);
  END;
END ReverseString;

PROCEDURE EncryptString(VAR Str: Text; Key: CARDINAL);
VAR I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(Str) DO
    Str[I] := Str[I] + Key;
  END;
END EncryptString;

PROCEDURE DecryptString(VAR Str: Text; Key: CARDINAL);
VAR I: CARDINAL;
BEGIN
  FOR I := 0 TO HIGH(Str) DO
    Str[I] := Str[I] - Key;
  END;
END DecryptString;

PROCEDURE DisplayMenu;
BEGIN
  TEXT("1. Make String Lowercase");
  TEXT("2. Remove Duplicate Characters");
  TEXT("3. Replace a Substring");
  TEXT("4. Title Case");
  TEXT("5. Format Text");
  TEXT("6. Convert to Morse Code");
  TEXT("7. Reverse String");
  TEXT("8. Encrypt String");
  TEXT("9. Decrypt String");
  TEXT("0. Quit");
END DisplayMenu;

PROCEDURE GetInput(VAR Input: Text);
BEGIN
  TEXT("Enter a string: ");
  READLN(Input);
END GetInput;

PROCEDURE TextProcessingMain;
VAR Input, Option: Text;
    Key: CARDINAL;
BEGIN
  LOOP
    DisplayMenu;
    TEXT("Enter an option: ");
    READLN(Option);
    CASE Option OF
      "1": GetInput(Input); MakeLower(Input); TEXT("Lowercase: "); WRITELN(Input);
      "2": GetInput(Input); RemoveDuplicates(Input); TEXT("Duplicates removed: "); WRITELN(Input);
      "3": GetInput(Input); TEXT("Enter substring to replace: "); READLN(TempText);
           TEXT("Enter replacement substring: "); READLN(TempText); ReplaceString(Input, TempText, TempText);
           TEXT("Replaced substring: "); WRITELN(Input);
      "4": GetInput(Input); TitleCase(Input); TEXT("Title case: "); WRITELN(Input);
      "5": GetInput(Input); FormatText(Input); TEXT("Formatted text: "); WRITELN(Input);
      "6": GetInput(Input); ConvertToMorseCode(Input); TEXT("Morse code: "); WRITELN(Input);
      "7": GetInput(Input); ReverseString(Input); TEXT("Reversed string: "); WRITELN(Input);
      "8": GetInput(Input); TEXT("Enter encryption key: "); READLN(Key); EncryptString(Input, Key);
           TEXT("Encrypted string: "); WRITELN(Input);
      "9": GetInput(Input); TEXT("Enter decryption key: "); READLN(Key); DecryptString(Input, Key);
           TEXT("Decrypted string: "); WRITELN(Input);
      ELSE: RETURN;
    END;
    TEXTNL;
  END;
END TextProcessingMain.
```

This code is a complex text processing program in Modula-2. It provides a menu-driven interface for performing various text transformations, including converting strings to lowercase, removing duplicate characters, replacing substrings, applying title case, formatting text, converting to Morse code, reversing strings, encrypting and decrypting strings.

The program uses several procedures and functions to perform these transformations:

- `MakeLower`: Converts a string to lowercase.

- `RemoveDuplicates`: Removes duplicate characters from a string.

- `ReplaceString`: Replaces a substring with another substring.

- `TitleCase`: Converts a string to title case.

- `FormatText`: Formats a string using format specifiers.

- `ConvertToMorseCode`: Converts a string to Morse code.

- `ReverseString`: Reverses the order of characters in a string.

- `EncryptString`: Encrypts a string using a simple encryption algorithm.

- `DecryptString`: Decrypts a string using the same encryption algorithm.

The `TextProcessingMain` procedure is the main entry point of the program. It displays a menu of options to the user and calls the appropriate procedure based on the user's input.

The program also includes several helper functions, such as `GetInput`, `DisplayMenu`, and `TEXTNL`, to handle input/output and formatting.

Overall, this program demonstrates the versatility of Modula-2 for text processing tasks.