```modula-2
MODULE ComplexCode;
FROM Terminal IMPORT WriteString, WriteLn;
FROM Strings IMPORT CopyString;
FROM TextSearch IMPORT BoyerMooreSearch;

TYPE
  ComplexRecord = RECORD
    name: ARRAY 20 OF CHAR;
    value: CARDINAL;
  END;

VAR
  complexArray: ARRAY 10 OF ComplexRecord;
  searchString: ARRAY 30 OF CHAR;

PROCEDURE InitComplexArray;
BEGIN
  FOR i := 0 TO 9 DO
    complexArray[i].name := "Record" + IntToStr(i);
    complexArray[i].value := i * 10;
  END;
END InitComplexArray;

PROCEDURE PrintComplexArray;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO 9 DO
    WriteString(complexArray[i].name);
    WriteString(": ");
    WriteLn(IntToStr(complexArray[i].value));
  END;
END PrintComplexArray;

PROCEDURE SearchForString;
VAR
  index: CARDINAL;
BEGIN
  CopyString(searchString, "Record7");
  index := BoyerMooreSearch(searchString, complexArray[0].name);
  IF index <> 0 THEN
    WriteString("Found ");
    WriteString(searchString);
    WriteString(" at index ");
    WriteLn(IntToStr(index));
  ELSE
    WriteString(searchString);
    WriteString(" not found");
    WriteLn;
  END;
END SearchForString;

BEGIN
  InitComplexArray;
  PrintComplexArray;
  SearchForString;
END ComplexCode.
```

Explanation:

1. Module Declaration:
   - The program is structured as a MODULA-2 module named ComplexCode.

2. Imports:
   - The program uses various modules from the MODULA-2 standard library:
     - Terminal: For input and output operations.
     - Strings: For string manipulation.
     - TextSearch: For the Boyer-Moore string search algorithm.

3. Data Types:
   - ComplexRecord: A custom record type representing complex data objects. It contains two fields: name (an array of 20 characters) and value (a cardinal integer).

4. Variables:
   - complexArray: An array of 10 ComplexRecord objects.
   - searchString: An array of 30 characters used for string searching.

5. Procedures:
   - InitComplexArray: Initializes the complexArray with sample data.
   - PrintComplexArray: Prints the contents of the complexArray to the console.
   - SearchForString: Performs a Boyer-Moore string search for a specific string (searchString) within the complexArray.

6. Main Program:
   - Calls the InitComplexArray procedure to initialize the data.
   - Calls the PrintComplexArray procedure to display the contents of the complexArray.
   - Calls the SearchForString procedure to search for a specific string in the complexArray.

This code demonstrates various aspects of MODULA-2 programming, including data structures, procedures, and modules. It also highlights the use of the Boyer-Moore string search algorithm from the standard library.