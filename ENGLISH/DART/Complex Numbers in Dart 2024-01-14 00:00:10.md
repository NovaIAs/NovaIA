```dart
import 'dart:math';

// Define a class to represent a complex number
class Complex {
  double real;
  double imaginary;

  // Constructor to initialize the complex number
  Complex(this.real, this.imaginary);

  // Add two complex numbers
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Subtract two complex numbers
  Complex operator -(Complex other) {
    return Complex(real - other.real, imaginary - other.imaginary);
  }

  // Multiply two complex numbers
  Complex operator *(Complex other) {
    return Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real);
  }

  // Divide two complex numbers
  Complex operator /(Complex other) {
    double denominator = other.real * other.real + other.imaginary * other.imaginary;
    return Complex(
        (real * other.real + imaginary * other.imaginary) / denominator,
        (imaginary * other.real - real * other.imaginary) / denominator);
  }

  // Find the magnitude of the complex number
  double magnitude() {
    return sqrt(real * real + imaginary * imaginary);
  }

  // Find the argument of the complex number
  double argument() {
    return atan2(imaginary, real);
  }

  // Convert the complex number to a string
  String toString() {
    return '$real + ${imaginary}i';
  }
}

// Define a function to generate a random complex number
Complex randomComplexNumber() {
  Random random = Random();
  return Complex(random.nextDouble() * 10, random.nextDouble() * 10);
}

// Main function to test the complex number class
void main() {
  // Create two random complex numbers
  Complex c1 = randomComplexNumber();
  Complex c2 = randomComplexNumber();

  // Print the two complex numbers
  print('c1 = $c1');
  print('c2 = $c2');

  // Add the two complex numbers
  Complex c3 = c1 + c2;

  // Print the result
  print('c1 + c2 = $c3');

  // Subtract the two complex numbers
  Complex c4 = c1 - c2;

  // Print the result
  print('c1 - c2 = $c4');

  // Multiply the two complex numbers
  Complex c5 = c1 * c2;

  // Print the result
  print('c1 * c2 = $c5');

  // Divide the two complex numbers
  Complex c6 = c1 / c2;

  // Print the result
  print('c1 / c2 = $c6');

  // Find the magnitude of the complex number c1
  double magnitude = c1.magnitude();

  // Print the result
  print('Magnitude of c1 = $magnitude');

  // Find the argument of the complex number c1
  double argument = c1.argument();

  // Print the result
  print('Argument of c1 = $argument');
}
```

**Explanation:**

This code defines a class called `Complex` to represent complex numbers. It includes operators for addition, subtraction, multiplication, and division, as well as methods to find the magnitude and argument of a complex number.

The `randomComplexNumber()` function generates a random complex number with a real and imaginary part between 0 and 10.

The `main()` function creates two random complex numbers, adds them, subtracts them, multiplies them, divides them, and finds the magnitude and argument of one of the complex numbers. It then prints the results to the console.

This code demonstrates how to work with complex numbers in Dart and provides a foundation for more advanced mathematical operations.