```dart
// This is a Dart program that generates a random number between 1 and 100.

import 'dart:math';

// Define the main function.
void main() {
  // Create a Random object.
  var random = new Random();

  // Generate a random number between 1 and 100.
  var randomNumber = random.nextInt(100) + 1;

  // Print the random number to the console.
  print(randomNumber);
}
```

**Explanation:**

* The `import 'dart:math';` statement imports the `dart:math` library, which contains the `Random` class.
* The `void main()` function is the entry point of the program.
* The `var random = new Random();` statement creates a new `Random` object.
* The `var randomNumber = random.nextInt(100) + 1;` statement generates a random number between 1 and 100. The `nextInt()` method takes an integer as an argument and returns a random integer between 0 and the argument minus 1. The `+ 1` is added to the result of `nextInt()` to make the range of random numbers from 1 to 100.
* The `print(randomNumber);` statement prints the random number to the console.