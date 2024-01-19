```modula-2

MODULE Sequences;
FROM FormatString IMPORT PrintString, PrintLn, PrintInt;
FROM Terminal IMPORT OpenConsole, ClrScr, HomeCursor, ReverseVideo,
                     NormalVideo, WaitChar, GetKey, Echo, NoEcho,
                     Beep;
FROM Strings IMPORT Length, SubString, Trim, StringToLong,
                     LongToString, TrimRight, UpperCase;
FROM Output IMPORT OpenFile, CloseFile, Write, WriteLn, Read, ReadLn, WriteString,
                     WriteChar, Error;
FROM Input IMPORT OpenTerminal, CloseTerminal, WriteString, ReadChar,
                     GetKey, ErrorStatus;
FROM Dates IMPORT Clock, ConvertDate;
FROM OperatingSystem IMPORT StandardError, StdOut;
FROM System IMPORT Addr, SizeOf, Ord;
FROM Ranges IMPORT Low, High;
FROM Strings IMPORT SubString, StringToLong, LongToString;

PROCEDURE DateTime(VAR Day, Month, Year, Hour, Minute, Second : LONGINT);
VAR
  DateVal, TimeVal : LONGINT;
BEGIN
  Clock(DateVal, TimeVal);
  ConvertDate(Day, Month, Year, DateVal);
  Hour := TimeVal DIV 3600000;
  Minute := TimeVal DIV 60000 MOD 60;
  Second := TimeVal MOD 60000;
END DateTime;

PROCEDURE SetBGColor(Color : LONGINT);
BEGIN
  IF Color = 0 THEN
    ClrScr
  ELSIF Color = 11 THEN
    ReverseVideo
  ELSE
    NormalVideo
  END
END SetBGColor;

PROCEDURE Popup(Text : ARRAY OF CHAR; Color : LONGINT);
VAR
  c : LONGINT;
BEGIN
  Echo(FALSE);
  NoEcho(FALSE);
  OpenConsole;
  SetBGColor(Color);
  WriteString(Text);
  WaitChar;
  ClrScr;
  CloseConsole;
END Popup;

PROCEDURE PrintTable(Title : ARRAY OF CHAR; ColumnHeaders : ARRAY OF ARRAY OF CHAR;
                      ColumnData : ARRAY OF ARRAY OF ARRAY OF CHAR);
TYPE
  TPos = LONGINT;
  TColor = ARRAY [0..15] OF BOOLEAN;
  TRect = RECORD
    Left, Top, Height, Width : TPos
  END;
  TCell = RECORD
    Data : STRING;
    Color : TColor
  END;
  TTable = ARRAY [0..0] OF TCell;
VAR
  Table : TTable;
  MaxRows, MaxCols : LONGINT;
  MaxCellWidth : TPos;
  Row, Col, I : LONGINT;
  Top : TPos;
  CellWidth : ARRAY [0..0] OF TPos;

  PROCEDURE FillCell(VAR Table : TTable; Row, Col : LONGINT; Data : STRING; Color : TColor);
  VAR
    Char : CHAR;
    I : LONGINT;
  BEGIN
    Table[Row][Col].Data := Data;
    Table[Row][Col].Color := Color;
    IF Length(Data) > CellWidth[Col] THEN
      CellWidth[Col] := Length(Data)
    END
  END FillCell;

  PROCEDURE DrawRect(VAR Rect : TRect; Color : TColor);
  VAR
    Pos : TPos;
  BEGIN
    SetBGColor(0);
    FOR Col := Rect.Left TO Rect.Left + Rect.Width DO
      FOR Row := Rect.Top TO Rect.Top + Rect.Height DO
        FillCell(Table, Row, Col, ' ', Color)
      END
    END;
    FOR I := 0 TO 15 DO
      IF Color[I] THEN
        SetBGColor(I)
      ELSE
        SetBGColor(0)
      END;
      FOR Row := Rect.Top TO Rect.Top + Rect.Height DO
        Char := ' ' + LongToString(I) + ' ';
        FillCell(Table, Row, Rect.Left, Char, Color)
      END
    END
  END DrawRect;

  PROCEDURE DrawTable(VAR Table : TTable; MaxRows, MaxCols : LONGINT; MaxCellWidth : TPos);
  VAR
    CellWidthSum : TPos;
    CellHeight : TPos;
    Char : CHAR;
    Color : TColor;
    I, J : LONGINT;
  BEGIN
    CellHeight := 1;
    CellWidthSum := 0;
    FOR I := 0 TO MaxCols DO
      CellWidthSum := CellWidthSum + CellWidth[I]
    END;
    CellWidthSum := CellWidthSum + 2 * MaxCols;
    Top := (80 - MaxRows * CellHeight) DIV 2;
    FOR I := 0 TO MaxCols DO
      Char := ' ' + LongToString(I) + ' ';
      FillCell(Table, 0, I, Char, [0..15])
    END;
    FOR Row := 1 TO MaxRows DO
      FillCell(Table, Row, 0, LongToString(Row - 1), [0..15])
    END;
    FOR Row := 0 TO MaxRows DO
      FOR Col := 0 TO MaxCols DO
        IF Table[Row][Col].Color MOD 16 = 0 THEN
          Color := Table[Row][Col].Color
        ELSE
          Color := Table[Row][Col].Color DIV 16
        END;
        SetBGColor(Color);
        WriteString(SubString(Table[Row][Col].Data, 1, CellWidth[Col]));
        IF Col < MaxCols THEN
          Write(' ')
        END
      END;
      WriteLn
    END;
    NormalVideo
  END DrawTable;

  PROCEDURE GetCell(Table : TTable; Row, Col : LONGINT) : STRING;
  VAR
    Result : STRING;
  BEGIN
    Result := Table[Row][Col].Data
  END GetCell;

  PROCEDURE SetCell(VAR Table : TTable; Row, Col : LONGINT; Value : STRING);
  BEGIN
    Table[Row][Col].Data := Value
  END SetCell;

BEGIN
  MaxRows := Length(ColumnHeaders);
  MaxCols := Length(ColumnHeaders[0]);
  MaxCellWidth := 0;
  FOR I := 0 TO MaxRows DO
    FOR J := 0 TO MaxCols DO
      CellWidth[J] := Length(ColumnHeaders[I][J])
    END
  END;
  Table := ARRAY [0..MaxRows] OF TCell;
  FOR Row := 0 TO MaxRows DO
    FOR Col := 0 TO MaxCols DO
      FillCell(Table, Row, Col, ColumnHeaders[Row][Col], [FALSE])
    END
  END;
  FOR Row := 1 TO MaxRows DO
    FOR Col := 0 TO MaxCols DO
      FillCell(Table, Row, Col, ColumnData[Row - 1][Col], [FALSE])
    END
  END;
  DrawTable(Table, MaxRows + 1, MaxCols, MaxCellWidth);
END PrintTable;

PROCEDURE Frame(Title : ARRAY OF CHAR);
VAR
  Date, Month, Year, Hour, Minute, Second : LONGINT;
  Key : CHAR;
BEGIN
  Echo(FALSE);
  NoEcho(FALSE);
  OpenConsole;
  DateTime(Date, Month, Year, Hour, Minute, Second);
  ClrScr;
  ReverseVideo;
  Write('  ');
  WriteString(TrimRight(Title, 76));
  Write('  ');
  NormalVideo;
  HomeCursor;
  WriteString(LongToString(Date) + '/' + LongToString(Month) + '/' + LongToString(Year) + '  ');
  WriteString(LongToString(Hour) + ':' + LongToString(Minute) + ':' + LongToString(Second));
  Beep;
  GetKey(Key);
  ClrScr;
  CloseConsole;
END Frame;

PROCEDURE Main;
VAR
  File : FILE;
  Char : CHAR;
  Answer : LONGINT;
  Table : ARRAY [0..2] OF ARRAY [0..2] OF CHAR;
  Text : ARRAY [0..255] OF CHAR;
BEGIN
  OpenTerminal;
  Frame('MAIN MENU');
  Write('1. View records');
  WriteLn;
  Write('2. Add record');
  WriteLn;
  Write('3. Edit record');
  WriteLn;
  Write('4. Delete record');
  WriteLn;
  Write('5. Exit');
  WriteLn;
  Write('Choice: ');
  ReadLn(Answer);
  WriteLn;
  CASE Answer OF
    1 : Frame('VIEW RECORDS');
      IF OpenFile('RECORDS.DAT', File, WRITE) THEN
        Error('Cannot open file RECORDS.DAT');
        CloseTerminal;
        HALT
      END;
      WHILE Read(File, Char) DO
        Write(Char)
      END;
      CloseFile(File)
    | 2 : Frame('ADD RECORD');
      IF OpenFile('RECORDS.DAT', File, WRITE) THEN
        Error('Cannot open file RECORDS.DAT');
        CloseTerminal;
        HALT
      END;
      Write('Name: ');
      ReadLn(Text);
      Write(File, Text);
      WriteLn(File);
      Write('Address: ');
      ReadLn(Text);
      Write(File, Text);
      WriteLn(File);
      Write('Phone: ');
      ReadLn(Text);
      Write(File, Text);
      WriteLn(File);
      CloseFile(File)
    | 3 : Frame('EDIT RECORD');
      Table[0][0] := '[Record Number]';
      Table[0][1] := '[Name]';
      Table[0][2] := '[Address]';
      Table[1][0] := '1';
      Table[1][1] := 'John Doe';
      Table[1][2] := '123 Main Street';
      Table[2][0] := '2';
      Table[2][1] := 'Jane Smith';
      Table[2][2] := '456 Elm Street';
      PrintTable('EDIT RECORDS', Table, Table);
      Write('Record Number: ');
      ReadLn(Answer);
      WriteLn;
      IF Answer = 1 THEN
        Write('New Name: ');
        ReadLn(Text);
        Table[1][1] := Text;
        Write('New Address: ');
        ReadLn(Text);
        Table[1][2] := Text
      ELSIF Answer = 2 THEN
        Write('New Name: ');
        ReadLn(Text);
        Table[2][1] := Text;
        Write('New Address: ');
        ReadLn(Text);
        Table[2][2] := Text
      ELSE
        Popup('Invalid Record Number');
        HALT
      END;
      PrintTable('EDIT RECORDS', Table, Table)
    | 4 : Frame('DELETE RECORD');
      Table[0][0] := '[Record Number]';
      Table[0][1] := '[Name]';
      Table[0][2] := '[Address]';
      Table[1][0] := '1';
      Table[1][1] := 'John Doe';
      Table[1][2] := '123 Main Street';
      Table[2][0] := '2';
      Table[2][1] := 'Jane Smith';
      Table[2][2] := '456 Elm Street';
      PrintTable('DELETE RECORDS',