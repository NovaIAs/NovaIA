```modula-2
MODULE StringFunctions;
FROM FormatString IMPORT Format, FormatF;
FROM Terminal IMPORT WriteCard, WriteLn, WriteString, ReadString;
FROM Strings IMPORT String;

TYPE CharSet = SET OF CHAR;
TYPE CharArray = ARRAY OF CHAR;

PROCEDURE PrintString(s: STRING);
BEGIN
  WriteString(s);
  WriteLn;
END PrintString;

PROCEDURE PrintCharArray(a: CharArray);
VAR i: CARDINAL;
BEGIN
  FOR i := LOW(a) TO HIGH(a) DO
    WriteCard(a[i]);
  OD;
  WriteLn;
END PrintCharArray;

PROCEDURE PrintCharSet(s: CharSet);
VAR i: CHAR;
BEGIN
  FOR i := LOW(s) TO HIGH(s) DO
    WriteCard(i);
  OD;
  WriteLn;
END PrintCharSet;

PROCEDURE PrintFormatted(s: STRING);
VAR f: ARRAY OF CHAR;
BEGIN
  Format(s, f);
  WriteString(f);
  WriteLn;
END PrintFormatted;

PROCEDURE PrintFormattedF(f: FORMAT, a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedF;

PROCEDURE PrintFormattedV(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedV;

PROCEDURE PrintFormattedFV(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedFV;

PROCEDURE PrintFormattedVF(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedVF;

PROCEDURE PrintFormattedVV(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedVV;

PROCEDURE PrintFormattedVVF(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedVVF;

PROCEDURE PrintFormattedVVF(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedVVF;

PROCEDURE PrintFormattedFVF(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedFVF;

PROCEDURE PrintFormattedFFV(f: FORMAT; VAR a: ARRAY OF CHAR);
VAR s: ARRAY OF CHAR;
BEGIN
  FormatF(f, s, a);
  WriteString(s);
  WriteLn;
END PrintFormattedFFV;

PROCEDURE PrintFormattedFV