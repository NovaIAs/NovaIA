```modula-2
MODULE ComplexModula2Code;

IMPORT InOut, Strings, Terminal, FormatString;

VAR
  s: ARRAY[0..9] OF STRING;
  i: INTEGER;
  c: CHAR;

PROCEDURE Insert(line: STRING);
  VAR
    p: POINTER TO STRING;
  BEGIN
    p := NEW(STRING);
    Strings.Assign(p^, line);
    s[i] := p;
    INC(i);
  END Insert;

PROCEDURE PrintIt;
  VAR
    j: INTEGER;
  BEGIN
    FOR j := 0 TO i - 1 DO
      InOut.WriteString(Terminal.output, s[j]);
    END;
  END PrintIt;

PROCEDURE DoReversedPrint;
  VAR
    j: INTEGER;
  BEGIN
    FOR j := i - 1 TO 0 BY -1 DO
      InOut.WriteString(Terminal.output, s[j]);
    END;
  END DoReversedPrint;

PROCEDURE DoReversedOrderStrLenPrint;
  TYPE
    StrLenRec = RECORD
      len: CARDINAL;
      p: POINTER TO STRING;
    END;
    StrLenPtr = POINTER TO StrLenRec;

    CompareFunc = FUNCTION(a, b: POINTER TO StrLenRec): INTEGER;
  VAR
    j: INTEGER;
    r: StrLenPtr;
    t: ARRAY[0..i-1] OF StrLenRec;
  BEGIN
    FOR j := 0 TO i - 1 DO
      t[j].p := s[j];
      t[j].len := Strings.StringLength(s[j]);
    END;

    r := t;

    Strings.QuickSort(r, i,
      FUNCTION(a, b: POINTER TO StrLenRec): INTEGER
        RETURN INTEGER(b^.len - a^.len)
      END);

    FOR j := i - 1 TO 0 BY -1 DO
      InOut.WriteString(Terminal.output, r^.p);
      INC(r);
    END;
  END DoReversedOrderStrLenPrint;

BEGIN
  i := 0;
  Insert("This is line 1");
  Insert("This is line 2");
  Insert("This is line 3");
  Insert("This is line 4");
  Insert("This is line 5");
  Insert("This is line 6");
  Insert("This is line 7");
  Insert("This is line 8");
  Insert("This is line 9");
  Insert("This is line 10");

  PrintIt;
  InOut.WriteString(Terminal.output, "\n");

  DoReversedPrint;
  InOut.WriteString(Terminal.output, "\n");

  DoReversedOrderStrLenPrint;
  InOut.WriteString(Terminal.output, "\n");

  WHILE Terminal.GetKeyStroke(c) DO
    IF c = 'E' THEN
      InOut.WriteLn(Terminal.output, "Good bye!");
      EXIT;
    END;
  END;

END ComplexModula2Code.
```

This code performs various operations on a list of strings (`s`).

- It starts by declaring an array `s` of 10 strings.
- It then uses a `FOR` loop to insert 10 strings into the `s` array.
- It defines three procedures: `PrintIt`, `DoReversedPrint`, and `DoReversedOrderStrLenPrint`.
- `PrintIt` simply prints all the strings in the `s` array in order.
- `DoReversedPrint` prints all the strings in the `s` array in reverse order.
- `DoReversedOrderStrLenPrint` prints all the strings in the `s` array in reverse order of their lengths.

The main program first calls `PrintIt` to print the strings in order. Then, it calls `DoReversedPrint` to print the strings in reverse order. Finally, it calls `DoReversedOrderStrLenPrint` to print the strings in reverse order of their lengths.

After performing these operations, the program waits for the user to press the `E` key. When the user presses `E`, the program prints "Good bye!" and exits.