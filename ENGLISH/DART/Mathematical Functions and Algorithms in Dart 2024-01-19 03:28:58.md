```dart
void main() {
  // Define a function to calculate the factorial of a number
  int factorial(int n) {
    if (n == 0) {
      return 1;
    } else {
      return n * factorial(n - 1);
    }
  }

  // Define a function to check if a number is prime
  bool isPrime(int n) {
    if (n <= 1) {
      return false;
    }
    for (int i = 2; i < n; i++) {
      if (n % i == 0) {
        return false;
      }
    }
    return true;
  }

  // Define a function to find the greatest common divisor of two numbers
  int gcd(int a, int b) {
    if (b == 0) {
      return a;
    } else {
      return gcd(b, a % b);
    }
  }

  // Define a function to find the least common multiple of two numbers
  int lcm(int a, int b) {
    return (a * b) ~/ gcd(a, b);
  }

  // Define a function to generate a random number between two numbers
  int random(int min, int max) {
    return min + Random().nextInt(max - min + 1);
  }

  // Define a function to print a list of numbers
  void printList(List<int> list) {
    for (int number in list) {
      print(number);
    }
  }

  // Create a list of numbers from 1 to 100
  List<int> numbers = List.generate(100, (i) => i + 1);

  // Print the list of numbers
  printList(numbers);

  // Find the factorial of 10
  int factorial10 = factorial(10);
  print("Factorial of 10: $factorial10");

  // Check if 17 is prime
  bool isPrime17 = isPrime(17);
  print("Is 17 prime: $isPrime17");

  // Find the greatest common divisor of 12 and 18
  int gcd12and18 = gcd(12, 18);
  print("Greatest common divisor of 12 and 18: $gcd12and18");

  // Find the least common multiple of 12 and 18
  int lcm12and18 = lcm(12, 18);
  print("Least common multiple of 12 and 18: $lcm12and18");

  // Generate a random number between 1 and 10
  int randomNumber = random(1, 10);
  print("Random number between 1 and 10: $randomNumber");
}
```

This code is a collection of different mathematical functions and algorithms implemented in Dart. It includes functions to calculate the factorial of a number, check if a number is prime, find the greatest common divisor of two numbers, find the least common multiple of two numbers, generate a random number between two numbers, and print a list of numbers.

The code is structured as follows:

1. **Function Definitions**: The code starts by defining several functions, each of which performs a different mathematical operation. These functions are:

   - `factorial`: Calculates the factorial of a number.
   - `isPrime`: Checks if a number is prime.
   - `gcd`: Finds the greatest common divisor of two numbers.
   - `lcm`: Finds the least common multiple of two numbers.
   - `random`: Generates a random number between two numbers.
   - `printList`: Prints a list of numbers.

2. **List of Numbers**: A list of numbers from 1 to 100 is created using the `List.generate` function.

3. **Printing the List**: The list of numbers is printed using the `printList` function.

4. **Factorial of 10**: The factorial of 10 is calculated using the `factorial` function and printed to the console.

5. **Primality of 17**: The primality of 17 is checked using the `isPrime` function and printed to the console.

6. **Greatest Common Divisor of 12 and 18**: The greatest common divisor of 12 and 18 is found using the `gcd` function and printed to the console.

7. **Least Common Multiple of 12 and 18**: The least common multiple of 12 and 18 is found using the `lcm` function and printed to the console.

8. **Random Number between 1 and 10**: A random number between 1 and 10 is generated using the `random` function and printed to the console.

This code demonstrates the use of various mathematical functions and algorithms in Dart. It can be used as a reference or starting point for implementing similar functions in your own projects.