```dart
// A complex and differentiated code in Dart:

// Importing necessary libraries
import 'dart:math';
import 'dart:async';
import 'dart:collection';

// Main function
void main() {
  // Creating a random number generator
  Random random = new Random();

  // Creating a list of integers
  List<int> numbers = new List<int>();

  // Populating the list with random integers
  for (int i = 0; i < 100; i++) {
    numbers.add(random.nextInt(1000));
  }

  // Printing the original list
  print("Original list: $numbers");

  // Sorting the list in ascending order
  numbers.sort((a, b) => a.compareTo(b));

  // Printing the sorted list
  print("Sorted list (ascending): $numbers");

  // Sorting the list in descending order
  numbers.sort((a, b) => b.compareTo(a));

  // Printing the sorted list
  print("Sorted list (descending): $numbers");

  // Creating a set of strings
  Set<String> strings = new Set<String>();

  // Populating the set with random strings
  for (int i = 0; i < 100; i++) {
    strings.add(random.nextInt(1000).toString());
  }

  // Printing the original set
  print("Original set: $strings");

  // Creating a queue of doubles
  Queue<double> doubles = new Queue<double>();

  // Populating the queue with random doubles
  for (int i = 0; i < 100; i++) {
    doubles.add(random.nextDouble());
  }

  // Printing the original queue
  print("Original queue: $doubles");

  // Creating a map of strings to integers
  Map<String, int> map = new Map<String, int>();

  // Populating the map with random key-value pairs
  for (int i = 0; i < 100; i++) {
    map[random.nextInt(1000).toString()] = random.nextInt(1000);
  }

  // Printing the original map
  print("Original map: $map");

  // Creating a function to calculate the factorial of a number
  int factorial(int n) {
    if (n == 0) {
      return 1;
    } else {
      return n * factorial(n - 1);
    }
  }

  // Printing the factorial of 10
  print("Factorial of 10: ${factorial(10)}");

  // Creating a function to check if a number is prime
  bool isPrime(int n) {
    if (n <= 1) {
      return false;
    } else {
      for (int i = 2; i <= sqrt(n); i++) {
        if (n % i == 0) {
          return false;
        }
      }
      return true;
    }
  }

  // Printing a list of prime numbers up to 100
  print("Prime numbers up to 100: ${[for (int i = 2; i <= 100; i++) if (isPrime(i)) i]}");

  // Creating a function to generate a random password
  String generatePassword(int length) {
    String password = "";
    for (int i = 0; i < length; i++) {
      password += random.nextInt(10).toString();
    }
    return password;
  }

  // Printing a randomly generated password of length 10
  print("Randomly generated password of length 10: ${generatePassword(10)}");

  // Creating a function to calculate the Fibonacci sequence up to a given number
  List<int> fibonacci(int n) {
    List<int> sequence = [0, 1];
    while (sequence[sequence.length - 1] < n) {
      int nextNumber = sequence[sequence.length - 1] + sequence[sequence.length - 2];
      sequence.add(nextNumber);
    }
    return sequence;
  }

  // Printing the Fibonacci sequence up to 100
  print("Fibonacci sequence up to 100: ${fibonacci(100)}");

  // Creating a function to reverse a string using recursion
  String reverseString(String str) {
    if (str.length == 1) {
      return str;
    } else {
      return reverseString(str.substring(1)) + str[0];
    }
  }

  // Printing the reversed string "Hello"
  print("Reversed string \"Hello\": ${reverseString("Hello")}");

  // Creating a function to find the longest common substring between two strings
  String longestCommonSubstring(String str1, String str2) {
    int[,] dp = new int[str1.length + 1][str2.length + 1];
    int maxLength = 0;
    int startIndex = 0;
    for (int i = 1; i <= str1.length; i++) {
      for (int j = 1; j <= str2.length; j++) {
        if (str1[i - 1] == str2[j - 1]) {
          dp[i][j] = dp[i - 1][j - 1] + 1;
          if (dp[i][j] > maxLength) {
            maxLength = dp[i][j];
            startIndex = i - maxLength;
          }
        }
      }
    }
    return str1.substring(startIndex, startIndex + maxLength);
  }

  // Printing the longest common substring between "Hello" and "World"
  print("Longest common substring between \"Hello\" and \"World\": ${longestCommonSubstring("Hello", "World")}");
}
```

This code showcases various features of the Dart programming language, including:

1. **Data Structures:**
   - `List<int>`: A list of integers
   - `Set<String>`: A set of strings
   - `Queue<double>`: A queue of doubles
   - `Map<String, int>`: A map of strings to integers

2. **Algorithms:**
   - **Sorting:** Sorting a list of integers in ascending and descending order
   - **Factorial Calculation:** Calculating the factorial of a number
   - **Primality Testing:** Checking if a number is prime
   - **Random Password Generation:** Generating a random password of a given length
   - **Fibonacci Sequence:** Calculating the Fibonacci sequence up to a given number
   - **String Reversal:** Reversing a string recursively
   - **Longest Common Substring:** Finding the longest common substring between two strings

3. **Functional Programming:** Using a lambda function to sort a list and a function expression to calculate the factorial of a number.

4. **Recursion:** Using recursion to reverse a string.

5. **Dynamic Programming:** Using dynamic programming to find the longest common substring between two strings.

This code demonstrates the versatility and expressiveness of the Dart language, showcasing its capabilities for complex and differentiated tasks.