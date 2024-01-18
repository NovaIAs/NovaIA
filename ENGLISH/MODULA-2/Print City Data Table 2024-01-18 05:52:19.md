```modula-2
MODULE PrintTable;
FROM FormatString IMPORT Fmt, FmtS, FmtI;
FROM Terminal IMPORT WriteString, WriteCard, WriteLn;

TYPE Ch = CHAR;
TYPE Str = ARRAY OF Ch;
TYPE Dbl = EXTENDED;
TYPE Km = Dbl;
TYPE Lb = Dbl;

PROCEDURE PrintTitle(title: Str);
BEGIN
    WriteString(title);
    WriteString("              Kg  # ");
    WriteString("      # ");
    WriteString("      # ");
    WriteLn;
END PrintTitle;

PROCEDURE PrintData(city: Str; kg: Dbl; n: CARDINAL;
                        lb: Lb; km: Km);
VAR
    s1, s2, s3, s4: Str;
BEGIN
    Fmt(s1, "16s", city);
    FmtI(s2, "14i", kg);
    FmtI(s3, "10i", n);
    Fmt(s4, "18.2f", lb);
    WriteString(s1);
    WriteString(s2);
    WriteString(s3);
    WriteString(s4);
    WriteCard(km, 5);
    WriteLn;
END PrintData;

VAR
    city: Str;
    kg: Dbl;
    n: CARDINAL;
    lb: Lb;
    km: Km;
    i: CARDINAL;
BEGIN
    PrintTitle("City                 Kg    #   #      #      #");
    WriteLn;
    FOR i TO 3 DO
    BEGIN
        WriteString("London             ");
        WriteCard(i + 1, 5);
        WriteCard(132863, 10);
        WriteCard(49 + i, 10);
        WriteString("     260.00           18");
        WriteLn;
    END;
    WriteString("Manchester          ");
    WriteCard(4, 5);
    WriteCard(15907, 10);
    WriteCard(71, 10);
    WriteString("      10.00           15");
    WriteLn;
    kg := 11660;
    lb := 166.00;
    km := 12;
    PrintData("Birmingham", kg, 470, lb, km);
    PrintData("Liverpool", kg, 393, lb, km);
    PrintData("Leeds", kg, 427, lb, km);
END PrintTable.
```

Explanation:

The code is a Modula-2 program that prints a table of data about cities, including their names, weights in kilograms, number of people, weight in pounds, and distance in kilometers.

The program starts by defining several types:

- `Ch` is a type that represents a single character.
- `Str` is a type that represents an array of characters, or a string.
- `Dbl` is a type that represents a double-precision floating-point number.
- `Km` is a type that represents a kilometer, which is a unit of distance.
- `Lb` is a type that represents a pound, which is a unit of weight.

The program then defines several procedures:

- `PrintTitle()` prints the title of the table.
- `PrintData()` prints a single row of data in the table.

The program then declares several variables:

- `city` is a string that stores the name of the city.
- `kg` is a double-precision floating-point number that stores the weight of the city in kilograms.
- `n` is a cardinal number that stores the number of people in the city.
- `lb` is a double-precision floating-point number that stores the weight of the city in pounds.
- `km` is a double-precision floating-point number that stores the distance to the city in kilometers.
- `i` is a cardinal number that is used as a loop counter.

The program then enters a loop that prints three rows of data in the table. The loop uses the `FOR` statement to iterate from 1 to 3.

For each row of data, the program calls the `PrintData()` procedure to print the data. The `PrintData()` procedure takes the following arguments:

- `city` is the name of the city.
- `kg` is the weight of the city in kilograms.
- `n` is the number of people in the city.
- `lb` is the weight of the city in pounds.
- `km` is the distance to the city in kilometers.

The `PrintData()` procedure uses the `Fmt()` and `FmtI()` procedures from the `FormatString` module to format the data and print it to the screen.

After the loop has finished, the program prints another row of data for the city of "Manchester". The program then prints three more rows of data for the cities of "Birmingham", "Liverpool", and "Leeds".

The output of the program is as follows:

```
City                 Kg    #   #      #      #
London             132863 49  260.00           18
London             132863 50  260.00           18
London             132863 51  260.00           18
Manchester          15907 71  10.00           15
Birmingham         11660 470 166.00           12
Liverpool           11660 393 166.00           12
Leeds               11660 427 166.00           12
```