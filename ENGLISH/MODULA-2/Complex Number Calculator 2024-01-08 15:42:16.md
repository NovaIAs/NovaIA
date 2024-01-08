```modula-2

MODULE Complex;
FROM FormatString IMPORT Format, WriteString, WriteF, WriteLn;
FROM Terminal IMPORT ReadChar, ReadCard, ReadInt;
FROM System IMPORT Addr, Halt, Ord, SizeOf;
FROM RealMath IMPORT Sqrt;

TYPE
    Complex = RECORD
        Real, Imaginary: REAL;
    END;

PROCEDURE ReadComplex(VAR C: Complex);
VAR
    R: ARRAY[0..31] OF CHAR;
BEGIN
    WriteString("Enter a complex number (e.g. 3+4i): ");
    ReadString(R);
    Format(R, "%f+%fi", [C.Real, C.Imaginary]);
END ReadComplex;

PROCEDURE WriteComplex(C: Complex);
BEGIN
    WriteString(Format("%10.2f+%10.2fi", [C.Real, C.Imaginary]));
END WriteComplex;

PROCEDURE WriteComplexLn(C: Complex);
BEGIN
    WriteComplex(C);
    WriteLn;
END WriteComplexLn;

PROCEDURE AddComplex(A, B: Complex): Complex;
BEGIN
    RETURN Complex(A.Real + B.Real, A.Imaginary + B.Imaginary);
END AddComplex;

PROCEDURE SubtractComplex(A, B: Complex): Complex;
BEGIN
    RETURN Complex(A.Real - B.Real, A.Imaginary - B.Imaginary);
END SubtractComplex;

PROCEDURE MultiplyComplex(A, B: Complex): Complex;
VAR
    R, I: REAL;
BEGIN
    R := A.Real * B.Real - A.Imaginary * B.Imaginary;
    I := A.Real * B.Imaginary + A.Imaginary * B.Real;
    RETURN Complex(R, I);
END MultiplyComplex;

PROCEDURE DivideComplex(A, B: Complex): Complex;
VAR
    R, I, D: REAL;
BEGIN
    D := B.Real * B.Real + B.Imaginary * B.Imaginary;
    R := (A.Real * B.Real + A.Imaginary * B.Imaginary) / D;
    I := (A.Imaginary * B.Real - A.Real * B.Imaginary) / D;
    RETURN Complex(R, I);
END DivideComplex;

PROCEDURE MagnitudeComplex(C: Complex): REAL;
BEGIN
    RETURN Sqrt(C.Real * C.Real + C.Imaginary * C.Imaginary);
END MagnitudeComplex;

PROCEDURE ConjugateComplex(C: Complex): Complex;
BEGIN
    RETURN Complex(C.Real, -C.Imaginary);
END ConjugateComplex;

PROCEDURE InverseComplex(C: Complex): Complex;
BEGIN
    RETURN DivideComplex(ConjugateComplex(C), MultiplyComplex(C, C));
END InverseComplex;

PROCEDURE PrintMenu;
BEGIN
    WriteString("Complex Number Calculator\n");
    WriteString("-------------------------\n");
    WriteString("1) Add two complex numbers\n");
    WriteString("2) Subtract two complex numbers\n");
    WriteString("3) Multiply two complex numbers\n");
    WriteString("4) Divide two complex numbers\n");
    WriteString("5) Find the magnitude of a complex number\n");
    WriteString("6) Find the conjugate of a complex number\n");
    WriteString("7) Find the inverse of a complex number\n");
    WriteString("8) Exit\n");
    WriteString("Enter your choice (1-8): ");
END PrintMenu;

PROCEDURE Main;
VAR
    Choice: CARD;
    A, B, R: Complex;
BEGIN
    REPEAT
        PrintMenu;
        ReadCard(Choice);
        CASE Choice OF
            1:
                WriteString("Enter the first complex number: ");
                ReadComplex(A);
                WriteString("Enter the second complex number: ");
                ReadComplex(B);
                R := AddComplex(A, B);
                WriteComplexLn(R);
            2:
                WriteString("Enter the first complex number: ");
                ReadComplex(A);
                WriteString("Enter the second complex number: ");
                ReadComplex(B);
                R := SubtractComplex(A, B);
                WriteComplexLn(R);
            3:
                WriteString("Enter the first complex number: ");
                ReadComplex(A);
                WriteString("Enter the second complex number: ");
                ReadComplex(B);
                R := MultiplyComplex(A, B);
                WriteComplexLn(R);
            4:
                WriteString("Enter the first complex number: ");
                ReadComplex(A);
                WriteString("Enter the second complex number: ");
                ReadComplex(B);
                R := DivideComplex(A, B);
                WriteComplexLn(R);
            5:
                WriteString("Enter a complex number: ");
                ReadComplex(A);
                WriteF("The magnitude of the complex number is %10.2f\n", [MagnitudeComplex(A)]);
            6:
                WriteString("Enter a complex number: ");
                ReadComplex(A);
                R := ConjugateComplex(A);
                WriteComplexLn(R);
            7:
                WriteString("Enter a complex number: ");
                ReadComplex(A);
                R := InverseComplex(A);
                WriteComplexLn(R);
            8:
                WriteLn("Goodbye!");
                Halt;
        ELSE
            WriteLn("Invalid choice. Please enter a number between 1 and 8.");
        END;
    UNTIL Choice = 8;
END Main.

```