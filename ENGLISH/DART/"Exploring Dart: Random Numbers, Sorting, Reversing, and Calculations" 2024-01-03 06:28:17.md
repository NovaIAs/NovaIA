Certainly! Here's a complex and differentiated Dart code that showcases various concepts and programming techniques:

```dart
import 'dart:math';

void main() {
  // Generate a list of random numbers
  List<int> numbers = generateRandomNumbers(10, 1, 100);
  
  // Print the original list
  print('Original List: $numbers');
  
  // Sort the list in ascending order
  numbers.sort();
  
  // Print the sorted list
  print('Sorted List (Ascending): $numbers');
  
  // Reverse the list
  List<int> reversedList = reverseList(numbers);
  
  // Print the reversed list
  print('Reversed List: $reversedList');
  
  // Calculate the sum of all numbers in the list
  int sum = calculateSum(numbers);
  
  // Calculate the average of all numbers in the list
  double average = calculateAverage(numbers);
  
  // Print the sum and average
  print('Sum: $sum');
  print('Average: $average');
  
  // Check if the list contains a specific number
  int targetNumber = 42;
  bool containsNumber = numbers.contains(targetNumber);
  
  // Print the result
  print('Does the list contain $targetNumber? $containsNumber');
}

List<int> generateRandomNumbers(int count, int min, int max) {
  Random random = Random();
  List<int> numbers = [];
  
  for (int i = 0; i < count; i++) {
    int randomNumber = random.nextInt(max - min + 1) + min;
    numbers.add(randomNumber);
  }
  
  return numbers;
}

List<int> reverseList(List<int> list) {
  List<int> reversedList = [];
  
  for (int i = list.length - 1; i >= 0; i--) {
    reversedList.add(list[i]);
  }
  
  return reversedList;
}

int calculateSum(List<int> list) {
  int sum = 0;
  
  for (int i = 0; i < list.length; i++) {
    sum += list[i];
  }
  
  return sum;
}

double calculateAverage(List<int> list) {
  int sum = calculateSum(list);
  return sum / list.length;
}
```

Explanation:

This Dart code demonstrates several fundamental concepts and techniques. Here's a breakdown:

1. The `generateRandomNumbers` function uses the `Random` class from the `dart:math` library to generate a list of random integers within a specified range.

2. The `reverseList` function reverses the order of elements in a given list by iterating from the last index to the first index and appending each element to a new list.

3. The `calculateSum` function calculates the sum of all numbers in a given list using a for loop.

4. The `calculateAverage` function calls the `calculateSum` function and divides the sum by the length of the list to calculate the average.

5. In the `main` function, we use the `generateRandomNumbers` function to generate a list of random numbers.

6. We print the original list, sort it in ascending order, and print the sorted list.

7. Next, the `reverseList` function is called to reverse the list, and the reversed list is printed.

8. The `calculateSum` function is called to calculate the sum of all numbers in the list, and the result is printed.

9. Similarly, the `calculateAverage` function is called to calculate the average, and the result is printed.

10. Finally, we check if the list contains a specific target number (`42` in this case) using the `contains` method, and print the result.

This code showcases the use of functions, loops, list manipulation, mathematical calculations, conditional statements, and library imports in Dart.