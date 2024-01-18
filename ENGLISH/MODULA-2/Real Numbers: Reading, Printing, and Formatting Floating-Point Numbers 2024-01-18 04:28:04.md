**MODULE RealNumbers;**

(**Includes all the definitions and declarations of the required modules. For example**:)

FROM FormatString IMPORT Writef, WriteLn;
FROM InOut IMPORT WriteString, ReadLine;
FROM RealIn IMPORT RealRead;
FROM RealOut IMPORT RealWrite;

**TYPE**
  Number = REAL;

**PROCEDURE**

  /************************/
  /* Reads a number from the standard input */
  /************************/

  FUNCTION ReadNumber() : Number;
  BEGIN
    NUMBER : RealRead();
  END ReadNumber;

  /****************************************/
  /* Prints a number to the standard output */
  /****************************************/

  PROCEDURE PrintNumber(Number : Number);
  BEGIN
    WriteString("[");
    RealWrite(Number,7);
    WriteString("]");
  END PrintNumber;

  /*******************************************/
  /* If the number is> 100 it prints it in red */
  /*******************************************/

  PROCEDURE PrintRedIfGreater100(Number : Number);
  BEGIN
    IF Number > 100.0 THEN
      WriteString("[");
      WriteString("#FF0000");
      RealWrite(Number,7);
      WriteString("#]");
    ELSE
     PrintNumber(Number);
    FI;
  END PrintRedIfGreater100;

  /*******************************/
  /* Main program: asks for a number and prints it */
  /*******************************/

BEGIN
  VAR
    X : Number;

  WriteString("Enter a number: ");
  X := ReadNumber();
  WriteLn;
  WriteString("The number entered is: ");
  PrintNumber(X);
  WriteLn;
  WriteString("The number entered is: ");
  PrintRedIfGreater100(X);
END RealNumbers.