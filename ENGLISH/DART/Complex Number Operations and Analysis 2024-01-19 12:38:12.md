```dart
// A complex and differentiated code in DART:

// Import the necessary libraries.
import 'dart:math';
import 'dart:collection';

// Define a class to represent a complex number.
class Complex {
  double real;
  double imaginary;

  // Constructor to initialize the complex number.
  Complex(this.real, this.imaginary);

  // Method to add two complex numbers.
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Method to subtract two complex numbers.
  Complex operator -(Complex other) {
    return Complex(real - other.real, imaginary - other.imaginary);
  }

  // Method to multiply two complex numbers.
  Complex operator *(Complex other) {
    return Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real);
  }

  // Method to divide two complex numbers.
  Complex operator /(Complex other) {
    double denominator = other.real * other.real + other.imaginary * other.imaginary;
    return Complex(
        (real * other.real + imaginary * other.imaginary) / denominator,
        (imaginary * other.real - real * other.imaginary) / denominator);
  }

  // Method to find the magnitude of a complex number.
  double magnitude() {
    return sqrt(real * real + imaginary * imaginary);
  }

  // Method to find the phase of a complex number.
  double phase() {
    return atan2(imaginary, real);
  }

  // Method to convert a complex number to a string.
  String toString() {
    return '($real + ${imaginary}i)';
  }
}

// Define a function to generate a random complex number.
Complex randomComplexNumber() {
  Random random = Random();
  return Complex(random.nextDouble(), random.nextDouble());
}

// Define a function to generate a list of random complex numbers.
List<Complex> generateRandomComplexNumbers(int count) {
  List<Complex> numbers = [];
  for (int i = 0; i < count; i++) {
    numbers.add(randomComplexNumber());
  }
  return numbers;
}

// Define a function to find the sum of a list of complex numbers.
Complex sumOfComplexNumbers(List<Complex> numbers) {
  Complex sum = Complex(0, 0);
  for (Complex number in numbers) {
    sum += number;
  }
  return sum;
}

// Define a function to find the average of a list of complex numbers.
Complex averageOfComplexNumbers(List<Complex> numbers) {
  Complex sum = sumOfComplexNumbers(numbers);
  return Complex(sum.real / numbers.length, sum.imaginary / numbers.length);
}

// Define a function to find the maximum magnitude of a list of complex numbers.
double maxMagnitudeOfComplexNumbers(List<Complex> numbers) {
  double maxMagnitude = 0;
  for (Complex number in numbers) {
    double magnitude = number.magnitude();
    if (magnitude > maxMagnitude) {
      maxMagnitude = magnitude;
    }
  }
  return maxMagnitude;
}

// Define a function to find the minimum magnitude of a list of complex numbers.
double minMagnitudeOfComplexNumbers(List<Complex> numbers) {
  double minMagnitude = double.infinity;
  for (Complex number in numbers) {
    double magnitude = number.magnitude();
    if (magnitude < minMagnitude) {
      minMagnitude = magnitude;
    }
  }
  return minMagnitude;
}

// Define a function to find the complex number with the maximum phase in a list of complex numbers.
Complex maxPhaseOfComplexNumbers(List<Complex> numbers) {
  double maxPhase = double.negativeInfinity;
  Complex maxPhaseNumber = Complex(0, 0);
  for (Complex number in numbers) {
    double phase = number.phase();
    if