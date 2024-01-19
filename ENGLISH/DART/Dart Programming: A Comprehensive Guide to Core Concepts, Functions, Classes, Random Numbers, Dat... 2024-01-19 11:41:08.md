// Import required libraries
import 'dart:math';
import 'dart:io';
import 'dart:collection';

// Define a main function
void main() {
  // Call a method to generate a random number
  int randomNumber = generateRandomNumber();

  // Print the random number
  print('Generated random number: $randomNumber');

  // Create a list of numbers
  List<int> numbers = [1, 2, 3, 4, 5];

  // Print the list of numbers
  print('List of numbers: $numbers');

  // Sort the list of numbers in ascending order
  numbers.sort();

  // Print the sorted list of numbers
  print('Sorted list of numbers: $numbers');

  // Create a map of key-value pairs
  Map<String, int> ages = {'Alice': 20, 'Bob': 25, 'Carol': 30};

  // Print the map of key-value pairs
  print('Map of key-value pairs: $ages');

  // Create a set of unique values
  Set<String> names = {'Alice', 'Bob', 'Carol', 'Dave', 'Eve'};

  // Print the set of unique values
  print('Set of unique values: $names');

  // Create a function to calculate the factorial of a number
  int factorial(int n) {
    if (n == 0) {
      return 1;
    }
    return n * factorial(n - 1);
  }

  // Call the factorial function with a number
  int result = factorial(5);

  // Print the result
  print('Factorial of 5: $result');

  // Create a function to calculate the area of a triangle
  double triangleArea(double base, double height) {
    return 0.5 * base * height;
  }

  // Call the triangleArea function with values
  double area = triangleArea(10.0, 5.0);

  // Print the result
  print('Area of triangle: $area');

  // Create a function to read a file
  String readFile(String filePath) {
    return File(filePath).readAsStringSync();
  }

  // Call the readFile function with a file path
  String fileContent = readFile('path/to/file.txt');

  // Print the file content
  print('File content: $fileContent');

  // Create a function to write to a file
  void writeFile(String filePath, String content) {
    File(filePath).writeAsStringSync(content);
  }

  // Call the writeFile function with a file path and content
  writeFile('path/to/file.txt', 'Hello World!');

  // Create a class to represent a person
  class Person {
    String name;
    int age;

    // Constructor
    Person(this.name, this.age);

    // Method to greet
    String greet() {
      return 'Hello, my name is $name and I am $age years old.';
    }
  }

  // Create an instance of the Person class
  Person person = Person('Alice', 20);

  // Call the greet method on the person instance
  String greeting = person.greet();

  // Print the greeting
  print('Greeting: $greeting');
}

// Method to generate a random number
int generateRandomNumber() {
  Random random = Random();
  return random.nextInt(100);
}