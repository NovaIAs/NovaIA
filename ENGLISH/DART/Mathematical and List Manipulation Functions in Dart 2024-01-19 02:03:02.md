```dart
// Import the necessary libraries.
import 'dart:io';
import 'dart:math';
import 'dart:async';

// Define a function to generate a random number between two values.
double randomNumber(double min, double max) {
  return min + (max - min) * Random().nextDouble();
}

// Define a function to calculate the area of a triangle.
double triangleArea(double base, double height) {
  return 0.5 * base * height;
}

// Define a function to calculate the volume of a sphere.
double sphereVolume(double radius) {
  return 4 / 3 * pi * pow(radius, 3);
}

// Define a function to calculate the distance between two points.
double distance(double x1, double y1, double x2, double y2) {
  return sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
}

// Define a function to find the largest element in a list.
double maxElement(List<double> list) {
  double max = list[0];
  for (double element in list) {
    if (element > max) {
      max = element;
    }
  }
  return max;
}

// Define a function to find the smallest element in a list.
double minElement(List<double> list) {
  double min = list[0];
  for (double element in list) {
    if (element < min) {
      min = element;
    }
  }
  return min;
}

// Define a function to calculate the average of a list of numbers.
double average(List<double> list) {
  double sum = 0;
  for (double element in list) {
    sum += element;
  }
  return sum / list.length;
}

// Define a function to sort a list of numbers in ascending order.
List<double> sortAscending(List<double> list) {
  list.sort((a, b) => a.compareTo(b));
  return list;
}

// Define a function to sort a list of numbers in descending order.
List<double> sortDescending(List<double> list) {
  list.sort((a, b) => b.compareTo(a));
  return list;
}

// Define a function to reverse a list of numbers.
List<double> reverse(List<double> list) {
  list.reversed;
  return list;
}

// Define a function to find the index of the first occurrence of an element in a list.
int findIndex(List<double> list, double element) {
  return list.indexOf(element);
}

// Define a function to find the index of the last occurrence of an element in a list.
int findLastIndex(List<double> list, double element) {
  return list.lastIndexOf(element);
}

// Define a function to remove all occurrences of an element from a list.
List<double> removeElement(List<double> list, double element) {
  list.remove(element);
  return list;
}

// Define a function to insert an element into a list at a specified index.
List<double> insertElement(List<double> list, double element, int index) {
  list.insert(index, element);
  return list;
}

// Define a function to print a list of numbers.
void printList(List<double> list) {
  for (double element in list) {
    print(element);
  }
}

// Get the user input.
print('Enter a list of numbers, separated by spaces:');
String input = stdin.readLineSync()!;

// Convert the input to a list of numbers.
List<double> numbers = input.split(' ').map((e) => double.parse(e)).toList();

// Calculate the area of a triangle with a base of 5 and a height of 3.
double triangleAreaResult = triangleArea(5, 3);

// Calculate the volume of a sphere with a radius of 10.
double sphereVolumeResult = sphereVolume(10);

// Calculate the distance between two points (1, 2) and (3, 4).
double distanceResult = distance(1, 2, 3, 4);

// Find the largest element in the list of numbers.
double maxElementResult = maxElement(numbers);

// Find the smallest element in the list of numbers.
double minElementResult = minElement(numbers);

// Calculate the average of the list of numbers.
double averageResult = average(numbers);

// Sort the list of numbers in ascending order.
List<double> sortedAscending = sortAscending(numbers);

// Sort the list of numbers in descending order.
List<double> sortedDescending = sortDescending(numbers);

// Reverse the list of numbers.
List<double> reversedList = reverse(numbers);

// Find the index of the first occurrence of the number 5 in the list of numbers.
int findIndexResult = findIndex(numbers, 5);

// Find the index of the last occurrence of the number 5 in the list of numbers.
int findLastIndexResult = findLastIndex(numbers, 5);

// Remove all occurrences of the number 5 from the list of numbers.
List<double> removedElement = removeElement(numbers, 5);

// Insert the number 10 into the list of numbers at