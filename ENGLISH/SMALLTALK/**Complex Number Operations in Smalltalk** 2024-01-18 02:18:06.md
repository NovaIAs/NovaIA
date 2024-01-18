```smalltalk
A Large and Differentiated Code in Smalltalk

Class Definition:
```
Object subclass: ComplexNumber [
  | real imaginary |
  initialize: realNum imaginaryNum [
    super initialize.
    real := realNum.
    imaginary := imaginaryNum.
  ]
  real: aReal [ real := aReal ]
  imaginary: anImaginary [ imaginary := anImaginary ]
  printOn: aStream [ aStream nextPutAll: (String with: real, ' + ', imaginary, 'i'). ]
  magnitude [ (real * real + imaginary * imaginary) sqrt ]
  add: anotherNumber [ self class new real: real + anotherNumber real imaginary: imaginary + anotherNumber imaginary ]
  subtract: anotherNumber [ self class new real: real - anotherNumber real imaginary: imaginary - anotherNumber imaginary ]
  multiply: anotherNumber [ self class new real: real * anotherNumber real - imaginary * anotherNumber imaginary imaginary: real * anotherNumber imaginary + imaginary * anotherNumber real ]
  divide: anotherNumber [ self class new real: ((real * anotherNumber real + imaginary * anotherNumber imaginary) / anotherNumber magnitude magnitude) imaginary: ((imaginary * anotherNumber real - real * anotherNumber imaginary) / anotherNumber magnitude magnitude) ]
]
```

Usage:
```
c1 := ComplexNumber new real: 3.0 imaginary: 4.0.
c2 := ComplexNumber new real: 2.0 imaginary: -1.0.

"Printing the complex numbers"
c1 printOn: Transcript.
Transcript cr.
c2 printOn: Transcript.
Transcript cr.

"Performing operations on complex numbers"
c3 := c1 add: c2.
"Adding c1 and c2"
Transcript show: 'Sum: '; c3 printString; cr.

c3 := c1 subtract: c2.
"Subtracting c2 from c1"
Transcript show: 'Difference: '; c3 printString; cr.

c3 := c1 multiply: c2.
"Multiplying c1 and c2"
Transcript show: 'Product: '; c3 printString; cr.

c3 := c1 divide: c2.
"Dividing c1 by c2"
Transcript show: 'Quotient: '; c3 printString; cr.

"Calculating the magnitude of c1"
Transcript show: 'Magnitude of c1: '; c1 magnitude printString; cr.
```

Explanation:

1. Class Definition: We define a class called ComplexNumber that represents complex numbers.

2. Instance Variables: Each ComplexNumber object has two instance variables: real and imaginary, representing the real and imaginary parts of the complex number, respectively.

3. Initialize Method: The initialize method initializes a ComplexNumber object with the provided real and imaginary parts.

4. Real and Imaginary Accessor Methods: The real: and imaginary: methods allow setting the real and imaginary parts of a ComplexNumber object.

5. Print Method: The printOn: method overrides the default printing behavior and prints the complex number in the format "real + imaginary i" to the specified stream.

6. Magnitude Method: The magnitude method calculates and returns the magnitude (absolute value) of the complex number.

7. Arithmetic Operation Methods:
   - add:, subtract:, multiply:, and divide: methods perform addition, subtraction, multiplication, and division operations between two complex numbers, respectively. These methods return new ComplexNumber objects representing the result of the operation.

8. Usage:
   - We create two ComplexNumber objects, c1 and c2, with specific real and imaginary parts.
   - We print the complex numbers to the Transcript.
   - We perform various arithmetic operations on c1 and c2 and print the results.
   - We also calculate and print the magnitude of c1.

This code demonstrates the creation and manipulation of complex numbers using a custom-defined class in Smalltalk.