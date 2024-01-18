```dart
import 'dart:math';

class ComplexNumber {
  final double real;
  final double imaginary;

  ComplexNumber(this.real, this.imaginary);

  ComplexNumber operator +(ComplexNumber other) {
    return ComplexNumber(real + other.real, imaginary + other.imaginary);
  }

  ComplexNumber operator -(ComplexNumber other) {
    return ComplexNumber(real - other.real, imaginary - other.imaginary);
  }

  ComplexNumber operator *(ComplexNumber other) {
    return ComplexNumber(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real);
  }

  ComplexNumber operator /(ComplexNumber other) {
    double denominator = other.real * other.real + other.imaginary * other.imaginary;
    return ComplexNumber(
        (real * other.real + imaginary * other.imaginary) / denominator,
        (imaginary * other.real - real * other.imaginary) / denominator);
  }

  double get magnitude => sqrt(real * real + imaginary * imaginary);

  double get argument => atan2(imaginary, real);

  ComplexNumber conjugate() => ComplexNumber(real, -imaginary);

  @override
  String toString() => '$real + ${imaginary}i';
}

void main() {
  ComplexNumber c1 = ComplexNumber(1, 2);
  ComplexNumber c2 = ComplexNumber(3, 4);

  print('c1 + c2 = ${c1 + c2}'); // ComplexNumber(4, 6)
  print('c1 - c2 = ${c1 - c2}'); // ComplexNumber(-2, -2)
  print('c1 * c2 = ${c1 * c2}'); // ComplexNumber(-5, 10)
  print('c1 / c2 = ${c1 / c2}'); // ComplexNumber(0.44, 0.08)

  print('c1.magnitude = ${c1.magnitude}'); // 2.23606797749979
  print('c1.argument = ${c1.argument}'); // 1.1071487177940904

  print('c1.conjugate() = ${c1.conjugate()}'); // ComplexNumber(1, -2)
}
```

This code defines a complex number class in Dart, which represents complex numbers with real and imaginary parts. It includes the basic arithmetic operations (+, -, *, /) for complex numbers, as well as methods to calculate the magnitude (absolute value), argument (angle), and conjugate of a complex number.

In the `main()` function, two complex numbers `c1` and `c2` are created and their arithmetic operations, magnitude, argument, and conjugate are calculated and printed to the console.

This code demonstrates the use of complex numbers in Dart, including complex arithmetic, properties, and methods. It can be used as a foundation for more advanced applications involving complex numbers, such as signal processing, electrical engineering, and quantum mechanics.