```modula-2

MODULE Fibonacci;
FROM FormatString IMPORT Printf;
FROM FormatLong IMPORT PrintLong;
FROM Bytecodes IMPORT CallBy, Ret;
FROM Assembler IMPORT AssemblerCall;
FROM INPUT IMPORT Read;
FROM Terminal IMPORT Write, WriteString, WriteLong;
FROM Timer IMPORT Delay;
FROM Stopwatch IMPORT StartStopwatch, GetStopwatch;

PROCEDURE OutputLine(s: ARRAY OF CHAR);
BEGIN
    Write(s);
    Write('\n');
END OutputLine;

TYPE Month: CARDINAL = (1..12);

PROCEDURE GetMonth(m: OUT Month);
VAR
    s: ARRAY OF CHAR;
    x: CARDINAL;
BEGIN
    OutputLine(['Introduce el número del mes (1-12): ']);
    Read(s);
    x:= 0;
    WHILE x < 1 OR x > 12 DO
        x:= StrToInt(s);
        IF x < 1 OR x > 12 THEN
            OutputLine(['Número de mes incorrecto. Inténtalo de nuevo: ']);
            Read(s);
        END;
    END;
    m:= x;
END GetMonth;

PROCEDURE WriteMonth(m: Month);
BEGIN
    CASE m OF
        1:  WriteString(['Enero']);
        2:  WriteString(['Febrero']);
        3:  WriteString(['Marzo']);
        4:  WriteString(['Abril']);
        5:  WriteString(['Mayo']);
        6:  WriteString(['Junio']);
        7:  WriteString(['Julio']);
        8:  WriteString(['Agosto']);
        9:  WriteString(['Septiembre']);
        10: WriteString(['Octubre']);
        11: WriteString(['Noviembre']);
        12: WriteString(['Diciembre']);
    END;
END WriteMonth;

PROCEDURE ShowMonths;
VAR
    m: Month;
BEGIN
    FOR m:= 1 TO 12 DO
        WriteMonth(m);
        Write('\n');
    END;
END ShowMonths;

PROCEDURE Main;
VAR
    m: Month;
BEGIN
    GetMonth(m);
    OutputLine(['El mes seleccionado es: ']);
    WriteMonth(m);
    OutputLine([]);
    Delay(1000);
    ShowMonths;
END Main.

```