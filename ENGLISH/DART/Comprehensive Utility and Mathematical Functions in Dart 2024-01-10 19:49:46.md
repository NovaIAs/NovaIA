```dart
// This Dart code demonstrates a complex and differentiated implementation of various functionalities.

// 1. Function to Find the Largest Number in an Array:
// This function takes an array of integers and returns the largest number in the array.
int findLargestNumber(List<int> numbers) {
  int largestNumber = numbers[0];
  for (int number in numbers) {
    if (number > largestNumber) {
      largestNumber = number;
    }
  }
  return largestNumber;
}

// 2. Function to Calculate the Factorial of a Number:
// This function calculates the factorial of a given non-negative integer.
int factorial(int number) {
  if (number == 0) {
    return 1;
  }
  int factorial = 1;
  for (int i = 1; i <= number; i++) {
    factorial *= i;
  }
  return factorial;
}

// 3. Function to Check if a String is a Palindrome:
// This function checks if a given string is a palindrome, meaning it reads the same forwards and backwards.
bool isPalindrome(String str) {
  String reversedStr = str.split('').reversed.join('');
  return str == reversedStr;
}

// 4. Function to Generate a Random String:
// This function generates a random string of a specified length.
String generateRandomString(int length) {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  Random random = Random();
  String randomString = '';
  for (int i = 0; i < length; i++) {
    randomString += chars[random.nextInt(chars.length)];
  }
  return randomString;
}

// 5. Function to Sort a List of Strings by Length:
// This function sorts a list of strings in ascending order based on their lengths.
List<String> sortByLength(List<String> strings) {
  strings.sort((a, b) => a.length.compareTo(b.length));
  return strings;
}

// 6. Class to Represent a Complex Number:
// This class represents complex numbers and provides methods for performing basic operations.
class ComplexNumber {
  double real;
  double imaginary;

  ComplexNumber(this.real, this.imaginary);

  ComplexNumber add(ComplexNumber other) {
    return ComplexNumber(real + other.real, imaginary + other.imaginary);
  }

  ComplexNumber subtract(ComplexNumber other) {
    return ComplexNumber(real - other.real, imaginary - other.imaginary);
  }

  ComplexNumber multiply(ComplexNumber other) {
    return ComplexNumber(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real);
  }

  ComplexNumber divide(ComplexNumber other) {
    double denominator = other.real * other.real + other.imaginary * other.imaginary;
    return ComplexNumber(
        (real * other.real + imaginary * other.imaginary) / denominator,
        (imaginary * other.real - real * other.imaginary) / denominator);
  }

  String toString() {
    return '($real + $imaginary i)';
  }
}

// 7. Function to Find the Roots of a Quadratic Equation:
// This function finds the roots of a quadratic equation of the form ax^2 + bx + c = 0.
List<double> findRootsOfQuadraticEquation(double a, double b, double c) {
  double discriminant = b * b - 4 * a * c;
  if (discriminant < 0) {
    return []; // No real roots
  } else {
    double root1 = (-b + sqrt(discriminant)) / (2 * a);
    double root2 = (-b - sqrt(discriminant)) / (2 * a);
    return [root1, root2];
  }
}

// 8. Function to Check if a Number is Prime:
// This function checks if a given number is prime, meaning it is only divisible by 1 and itself.
bool isPrime(int number) {
  if (number <= 1) {
    return false;
  }
  for (int i = 2; i <= number / 2; i++) {
    if (number % i == 0) {
      return false;
    }
  }
  return true;
}

// 9. Function to Find the GCD of Two Numbers:
// This function finds the greatest common divisor (GCD) of two given numbers.
int gcd(int a, int b) {
  while (b != 0) {
    int temp = b;
    b = a % b;
    a = temp;
  }
  return a;
}

// 10. Function to Find the LCM of Two Numbers:
// This function finds the least common multiple (LCM) of two given numbers.
int lcm(int a, int b) {
  return (a * b) ~/ gcd(a, b);
}

// Usage of the Functions:
void main() {
  // 1. Find the largest number in an array:
  List<int> numbers = [1, 5, 2, 8, 3];
  int largest = findLargestNumber(numbers);
  print('Largest Number: $largest');

  // 2. Calculate the factorial of a number:
  int number = 5;
  int factorialValue = factorial(number);
  print('Factorial of $number: $factorialValue');

  // 3. Check if a string is a palindrome:
  String str = 'racecar';
  bool isPalindromeResult = isPalindrome(str);
  print('Is "$str" a palindrome? $isPalindromeResult');

  // 4. Generate a random string:
  int length = 10;
  String randomString = generateRandomString(length);
  print('Random String: $randomString');

  // 5. Sort a list of strings by length:
  List<String> strings = ['grape', 'apple', 'banana', 'cherry', 'orange'];
  List<String> sortedStrings = sortByLength(strings);
  print('Sorted Strings by Length: $sortedStrings');

  // 6. Complex number operations:
  ComplexNumber c1 = ComplexNumber(3, 4);
  ComplexNumber c2 = ComplexNumber(5, -2);
  print('Complex Number 1: $c1');
  print('Complex Number 2: $c2');
  print('Addition: ${c1.add(c2)}');
  print('Subtraction: ${c1.subtract(c2)}');
  print('Multiplication: ${c1.multiply(c2)}');
  print('Division: ${c1.divide(c2)}');

  // 7. Find roots of a quadratic equation:
  double a = 1;
  double b = -5;
  double c = 6;
  List<double> roots = findRootsOfQuadraticEquation(a, b, c);
  print('Roots of Quadratic Equation: $roots');

  // 8. Check if a number is prime:
  int numberToCheck = 17;
  bool isPrimeResult = isPrime(numberToCheck);
  print('Is $numberToCheck prime? $isPrimeResult');

  // 9. Find the GCD of two numbers:
  int num1 = 12;
  int num2 = 18;
  int gcdResult = gcd(num1, num2);
  print('GCD of $num1 and $num2: $gcdResult');

  // 10. Find the LCM of two numbers:
  int lcmResult = lcm(num1, num2);
  print('LCM of $num1 and $num2: $lcmResult');
}
```

Explanation:

1. The `findLargestNumber` function iterates through an array of integers and returns the largest number.

2. The `factorial` function recursively calculates the factorial of a non-negative integer.

3. The `isPalindrome` function checks if a string reads the same forwards and backwards.

4. The `generateRandomString` function generates a random string of a specified length.

5. The `sortByLength` function sorts a list of strings in ascending order based on their lengths.

6. The `ComplexNumber` class represents complex numbers and provides methods for performing basic operations.

7. The `findRootsOfQuadraticEquation` function finds the roots of a quadratic equation using the quadratic formula.

8. The `isPrime` function checks if a given number is prime.

9. The `gcd` function finds the greatest common divisor (GCD) of two numbers using the Euclidean algorithm.

10. The `lcm` function finds the least common multiple (LCM) of two numbers using the GCD.

The `main` function demonstrates the usage of these functions by performing various calculations and operations.