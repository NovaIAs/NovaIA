```visual basic
Option Explicit

' Define a custom data type to represent a complex number.
Type Complex
    Real As Double
    Imaginary As Double
End Type

' Define a function to add two complex numbers.
Function AddComplex(ByVal a As Complex, ByVal b As Complex) As Complex
    Dim c As Complex
    c.Real = a.Real + b.Real
    c.Imaginary = a.Imaginary + b.Imaginary
    AddComplex = c
End Function

' Define a function to subtract two complex numbers.
Function SubtractComplex(ByVal a As Complex, ByVal b As Complex) As Complex
    Dim c As Complex
    c.Real = a.Real - b.Real
    c.Imaginary = a.Imaginary - b.Imaginary
    SubtractComplex = c
End Function

' Define a function to multiply two complex numbers.
Function MultiplyComplex(ByVal a As Complex, ByVal b As Complex) As Complex
    Dim c As Complex
    c.Real = a.Real * b.Real - a.Imaginary * b.Imaginary
    c.Imaginary = a.Real * b.Imaginary + a.Imaginary * b.Real
    MultiplyComplex = c
End Function

' Define a function to divide two complex numbers.
Function DivideComplex(ByVal a As Complex, ByVal b As Complex) As Complex
    Dim c As Complex
    If b.Real = 0 And b.Imaginary = 0 Then
        Err.Raise 11, "DivideComplex", "Division by zero."
    End If
    c.Real = (a.Real * b.Real + a.Imaginary * b.Imaginary) / (b.Real ^ 2 + b.Imaginary ^ 2)
    c.Imaginary = (a.Imaginary * b.Real - a.Real * b.Imaginary) / (b.Real ^ 2 + b.Imaginary ^ 2)
    DivideComplex = c
End Function

' Define a sub procedure to print a complex number.
Sub PrintComplex(ByVal c As Complex)
    Print c.Real; " + "; c.Imaginary; "i"
End Sub

' Test the complex number functions.
Dim a As Complex
Dim b As Complex
Dim c As Complex

a.Real = 3
a.Imaginary = 4
b.Real = 5
b.Imaginary = -2

c = AddComplex(a, b)
PrintComplex c
c = SubtractComplex(a, b)
PrintComplex c
c = MultiplyComplex(a, b)
PrintComplex c
c = DivideComplex(a, b)
PrintComplex c
```

This code defines a custom data type to represent a complex number, and then defines functions to add, subtract, multiply, and divide two complex numbers. It also defines a sub procedure to print a complex number. Finally, it tests the complex number functions by creating two complex numbers, adding them together, subtracting them, multiplying them, and dividing them, and then printing the results.

Here is a breakdown of the code:

* The `Option Explicit` statement forces you to explicitly declare all variables before using them. This helps to catch errors early.
* The `Type Complex` statement defines a custom data type named `Complex`. This data type has two fields: `Real` and `Imaginary`.
* The `AddComplex`, `SubtractComplex`, `MultiplyComplex`, and `DivideComplex` functions perform the specified operations on two complex numbers.
* The `PrintComplex` sub procedure prints a complex number to the console.
* The `Test the complex number functions` section of the code creates two complex numbers, `a` and `b`, and then uses the complex number functions to add, subtract, multiply, and divide them. The results are then printed to the console.