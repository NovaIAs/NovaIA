```modula-2
MODULE ComplexCode;
FROM Terminal IMPORT WriteString, WriteLn, WriteCard;
FROM Strings IMPORT Length, CopyString;
FROM Random IMPORT Random;

TYPE
  CharSet = SET OF CHAR;

PROCEDURE ShowSet(s: CharSet);
VAR i: CARDINAL;
BEGIN
  WriteString("{");
  FOR i:= LOW(s) TO HIGH(s) DO
    IF i IN s THEN WriteCard(i) END
  END;
  WriteLn("}");
END ShowSet;

PROCEDURE ShowString(s: ARRAY OF CHAR);
VAR i: CARDINAL;
BEGIN
  WriteString("'");
  FOR i:= LOW(s) TO HIGH(s) DO
    WriteString(s[i])
  END;
  WriteLn("'");
END ShowString;

PROCEDURE CreateRandomSet(n: CARDINAL): CharSet;
VAR i: CARDINAL;
    s: CharSet;
BEGIN
  s := {};
  FOR i:= 1 TO n DO
    s := s + CHR(Random(255))
  END;
  RETURN s
END CreateRandomSet;

PROCEDURE CreateRandomString(n: CARDINAL): ARRAY OF CHAR;
VAR i: CARDINAL;
    s: ARRAY OF CHAR;
BEGIN
  NEW(s, n);
  FOR i:= LOW(s) TO HIGH(s) DO
    s[i] := CHR(Random(255))
  END;
  RETURN s
END CreateRandomString;

PROCEDURE Test1;
VAR s1, s2: CharSet;
BEGIN
  s1 := CreateRandomSet(10);
  s2 := CreateRandomSet(15);
  WriteString("s1 = "); ShowSet(s1);
  WriteString("s2 = "); ShowSet(s2);
  WriteString("s1 + s2 = "); ShowSet(s1 + s2);
  WriteString("s1 - s2 = "); ShowSet(s1 - s2);
  WriteString("s1 * s2 = "); ShowSet(s1 * s2);
  WriteString("s1 << 2 = "); ShowSet(s1 << 2);
  WriteString("s1 >> 3 = "); ShowSet(s1 >> 3)
END Test1;

PROCEDURE Test2;
VAR s1, s2: ARRAY OF CHAR;
BEGIN
  s1 := CreateRandomString(10);
  s2 := CreateRandomString(15);
  WriteString("s1 = "); ShowString(s1);
  WriteString("s2 = "); ShowString(s2);
  WriteString("s1 + s2 = "); ShowString(s1 + s2);
  WriteString("s1 & s2 = "); ShowString(s1 & s2);
  WriteString("s1 - s2 = "); ShowString(s1 - s2);
  WriteString("s1 * s2 = "); ShowString(s1 * s2);
  WriteString("s1 << 2 = "); ShowString(s1 << 2);
  WriteString("s1 >> 3 = "); ShowString(s1 >> 3)
END Test2;

BEGIN
  Test1;
  Test2
END ComplexCode.
```

This code performs a series of operations on character sets and strings in MODULA-2. It includes the following features:

1. **Set Operations:** It defines a type `CharSet`, which represents a set of characters, and demonstrates various set operations such as union (`+`), intersection (`*`), difference (`-`), and shifting (`<<` and `>>`).

2. **String Operations:** It demonstrates string operations like concatenation (`+`), appending (`&`), subtraction (`-`), multiplication (`*`), and shifting (`<<` and `>>`).

3. **Random Data Generation:** It uses the `Random` module to generate random character sets and strings, ensuring that the operations are tested on a variety of data.

4. **User-Defined Procedures:** It defines two procedures, `Test1` and `Test2`, which perform the set and string operations, respectively.

5. **Output Formatting:** It uses `WriteString` and `WriteCard` from the `Terminal` module to display the results of the operations in a clear and structured manner.

This code showcases the versatility and expressive power of MODULA-2 for manipulating sets and strings. It also highlights the use of random data generation for testing purposes, making it a comprehensive and practical example of MODULA-2 programming.