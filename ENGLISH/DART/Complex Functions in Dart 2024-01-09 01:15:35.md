```dart
import 'package:flutter/material.dart';

class ComplexDartCode extends StatefulWidget {
  @override
  _ComplexDartCodeState createState() => _ComplexDartCodeState();
}

class _ComplexDartCodeState extends State<ComplexDartCode> {
  // A complex function that takes a list of integers and returns the sum of the squares of the integers
  int sumOfSquares(List<int> numbers) {
    int sum = 0;
    for (int number in numbers) {
      sum += number * number;
    }
    return sum;
  }

  // A complex function that takes a list of strings and returns a list of the strings in reverse order
  List<String> reverseStrings(List<String> strings) {
    List<String> reversedStrings = [];
    for (int i = strings.length - 1; i >= 0; i--) {
      reversedStrings.add(strings[i]);
    }
    return reversedStrings;
  }

  // A complex function that takes a list of integers and returns a list of the integers sorted in descending order
  List<int> sortIntegersDescending(List<int> numbers) {
    numbers.sort((a, b) => b.compareTo(a));
    return numbers;
  }

  // A complex function that takes a list of strings and returns a map of the strings to their lengths
  Map<String, int> countStringLengths(List<String> strings) {
    Map<String, int> stringLengths = {};
    for (String string in strings) {
      stringLengths[string] = string.length;
    }
    return stringLengths;
  }

  // A complex function that takes a list of integers and returns a list of the integers that are greater than a given threshold
  List<int> filterIntegersGreaterThanThreshold(List<int> numbers, int threshold) {
    List<int> filteredNumbers = [];
    for (int number in numbers) {
      if (number > threshold) {
        filteredNumbers.add(number);
      }
    }
    return filteredNumbers;
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Complex Dart Code'),
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Text('Sum of squares: ${sumOfSquares([1, 2, 3, 4, 5])}'),
            Text('Reversed strings: ${reverseStrings(['Hello', 'World', '!'])}'),
            Text('Sorted integers descending: ${sortIntegersDescending([1, 2, 3, 4, 5])}'),
            Text('String lengths: ${countStringLengths(['Hello', 'World', '!'])}'),
            Text('Filtered integers greater than 3: ${filterIntegersGreaterThanThreshold([1, 2, 3, 4, 5], 3)}'),
          ],
        ),
      ),
    );
  }
}
```

This code demonstrates several complex functions in Dart:

1. `sumOfSquares`: This function takes a list of integers and returns the sum of the squares of the integers. For example, `sumOfSquares([1, 2, 3, 4, 5])` returns 55.

2. `reverseStrings`: This function takes a list of strings and returns a list of the strings in reverse order. For example, `reverseStrings(['Hello', 'World', '!'])` returns ['!', 'World', 'Hello'].

3. `sortIntegersDescending`: This function takes a list of integers and returns a list of the integers sorted in descending order. For example, `sortIntegersDescending([1, 2, 3, 4, 5])` returns [5, 4, 3, 2, 1].

4. `countStringLengths`: This function takes a list of strings and returns a map of the strings to their lengths. For example, `countStringLengths(['Hello', 'World', '!'])` returns {'Hello': 5, 'World': 5, '!': 1}.

5. `filterIntegersGreaterThanThreshold`: This function takes a list of integers and a threshold value, and returns a list of the integers that are greater than the threshold. For example, `filterIntegersGreaterThanThreshold([1, 2, 3, 4, 5], 3)` returns [4, 5].

The `ComplexDartCode` class is a Flutter widget that displays the results of these functions in a user interface. The `build` method creates a `Scaffold` widget with an `AppBar` and a `Center` widget. The `Center` widget contains a `Column` widget with several `Text` widgets that display the results of the functions.

To use this code, you can create a new Flutter project and add the `ComplexDartCode` class to the `lib` directory. Then, you can add the `ComplexDartCode` widget to the `build` method of your `MyApp` class. When you run the app, you will see the results of the complex functions displayed in the user interface.