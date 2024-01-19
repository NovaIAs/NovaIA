```smalltalk

// Define the class 'ComplexNumber' to represent complex numbers.
ComplexNumber := Class new.

// Define instance variables for the real and imaginary parts of the complex number.
ComplexNumber instVarNames: 'real imaginary'

// Define a constructor method to initialize a complex number.
ComplexNumber class >> new: aReal aImaginary [
    ^ self new
        real: aReal;
        imaginary: aImaginary
]

// Define methods to access the real and imaginary parts of the complex number.
ComplexNumber class >> real [
    ^ self real
]

ComplexNumber class >> imaginary [
    ^ self imaginary
]

// Define a method to add two complex numbers together.
ComplexNumber class >> +: otherComplexNumber [
    ^ self new
        real: self real + otherComplexNumber real;
        imaginary: self imaginary + otherComplexNumber imaginary
]

// Define a method to subtract two complex numbers.
ComplexNumber class >> -: otherComplexNumber [
    ^ self new
        real: self real - otherComplexNumber real;
        imaginary: self imaginary - otherComplexNumber imaginary
]

// Define a method to multiply two complex numbers together.
ComplexNumber class >> *: otherComplexNumber [
    ^ self new
        real: (self real * otherComplexNumber real) - (self imaginary * otherComplexNumber imaginary);
        imaginary: (self real * otherComplexNumber imaginary) + (self imaginary * otherComplexNumber real)
]

// Define a method to divide two complex numbers.
ComplexNumber class >> /: otherComplexNumber [
    | denominator |
    denominator := (otherComplexNumber real ^ 2) + (otherComplexNumber imaginary ^ 2);
    ^ self new
        real: ((self real * otherComplexNumber real) + (self imaginary * otherComplexNumber imaginary)) / denominator;
        imaginary: ((self imaginary * otherComplexNumber real) - (self real * otherComplexNumber imaginary)) / denominator
]

// Define a method to compute the absolute value of a complex number.
ComplexNumber class >> abs [
    ^ Math sqrt: (self real ^ 2) + (self imaginary ^ 2)
]

// Define a method to compute the argument of a complex number.
ComplexNumber class >> arg [
    ^ Math atan2: self imaginary, self real
]

// Define a method to return a string representation of a complex number.
ComplexNumber class >> printString [
    ^ String streamContents: [ :stream |
        stream nextPutAll: (self real printString);
        stream nextPut: $' ';
        stream nextPutAll: (self imaginary printString);
        stream nextPutAll: 'i'
    ]
]

// Create an instance of the 'ComplexNumber' class.
z := ComplexNumber new: 3.0: 4.0.

// Display the complex number on the console.
Transcript show: z printString.

```

Explanation:

1. Class Definition (`ComplexNumber`):
   - We define a class named `ComplexNumber` to represent complex numbers.

2. Instance Variables:
   - We define two instance variables, `real` and `imaginary`, to store the real and imaginary parts of the complex number.

3. Constructor Method (`new:`):
   - We define a constructor method `new:` that takes two arguments, `aReal` and `aImaginary`, and initializes a new `ComplexNumber` object with these values.

4. Accessor Methods (`real` and `imaginary`):
   - We define accessor methods `real` and `imaginary` to retrieve the real and imaginary parts of the complex number.

5. Arithmetic Operations (`+`, `-`, `*`, `/`):
   - We define methods for performing basic arithmetic operations on complex numbers: addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`).

6. Absolute Value Method (`abs`):
   - We define a method `abs` to compute the absolute value (magnitude) of a complex number.

7. Argument Method (`arg`):
   - We define a method `arg` to compute the argument (angle) of a complex number.

8. String Representation Method (`printString`):
   - We define a method `printString` to return a string representation of a complex number.

9. Creating an Instance (`z`):
   - We create an instance of the `ComplexNumber` class named `z` with a real part of 3.0 and an imaginary part of 4.0.

10. Displaying the Complex Number:
    - We use the `Transcript` class to display the string representation of the complex number `z` on the console.