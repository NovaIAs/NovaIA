```dart
import 'dart:async' show Future;
import 'dart:convert' show json;
import 'dart:math' show Random;

/// A one-line comment.
///
/// Multi-line comments can also be used.

// Generates a random integer between [min] and [max].
int randInt(int min, int max) => min + Random().nextInt(max - min);

// Defines a Point class.
class Point {
  final int x, y;

  // The constructor.
  Point(this.x, this.y);

  Point operator +(Point other) => Point(x + other.x, y + other.y);

  @override
  String toString() => 'Point($x, $y)';
}

// Defines a Rectangle class.
class Rectangle {
  final Point topLeft, bottomRight;

  // The constructor.
  Rectangle(this.topLeft, this.bottomRight);

  // Computes the area of the rectangle.
  int get area => (bottomRight.x - topLeft.x) * (bottomRight.y - topLeft.y);

  // Computes the perimeter of the rectangle.
  int get perimeter =>
      2 * (bottomRight.x - topLeft.x + bottomRight.y - topLeft.y);

  @override
  String toString() =>
      'Rectangle($topLeft, $bottomRight) area=$area perimeter=$perimeter';
}

// Main function: entry point of the program.
Future<void> main() async {
  // Variables declaration.
  const int x = 10;
  int y = 20;
  var z = 'Hello, World!';

  // Variables assignment.
  x = 100; // Error: variable 'x' is immutable.
  y = 30;
  z = 'Goodbye, World!';

  // Creating a Point.
  Point point = Point(1, 2);

  // Creating a Rectangle.
  Rectangle rectangle = Rectangle(Point(0, 0), Point(10, 10));

  // Printing values.
  print('x: $x');
  print('y: $y');
  print('z: $z');
  print('point: $point');
  print('rectangle: $rectangle');

  // If statement.
  if (x > 0) {
    // Do something.
  } else if (x == 0) {
    // Do something else.
  } else {
    // Do something different.
  }

  // For loop.
  for (var i = 0; i < 10; i++) {
    // Do something.
  }

  // While loop.
  var count = 0;
  while (count < 10) {
    // Do something.
    count++;
  }

  // Do-while loop.
  count = 0;
  do {
    // Do something.
    count++;
  } while (count < 10);

  // Switch statement.
  switch (x) {
    case 0:
      // Do something.
      break;
    case 1:
      // Do something else.
      break;
    default:
      // Do something different.
      break;
  }

  // Try-catch statement.
  try {
    // Do something.
  } catch (e) {
    // Handle the exception.
  } finally {
    // Always called, regardless of whether an exception was thrown or not.
  }

  // Function declaration.
  String greet(String name) => 'Hello, $name!';

  // Function call.
  var greeting = greet('Alice');

  // Arrow function.
  int sum(int x, int y) => x + y;

  // Closures.
  var add = (int x) => (int y) => x + y;
  var add10 = add(10);
  var result = add10(20);

  // Anonymous function.
  var anonymousFunction = (int x, int y) => x + y;

  // Higher-order functions.
  List<int> numbers = [1, 2, 3, 4, 5];
  var sumOfNumbers = numbers.fold(0, (int sum, int number) => sum + number);

  // List comprehension.
  var evenNumbers = [for (var number in numbers) if (number % 2 == 0) number];

  // Set comprehension.
  var uniqueNumbers = {for (var number in numbers) number};

  // Map comprehension.
  var numbersMap = {for (var number in numbers) number: number * 2};

  // Generator function.
  Iterable<int> generateNumbers(int n) sync* {
    for (var i = 0; i < n; i++) yield i;
  }

  for (var number in generateNumbers(10)) {
    // Do something.
  }

  // Async function.
  Future<int> asyncFunction() async {
    // Do something asynchronous.
    return 42;
  }

  var result = await asyncFunction();

  // Error handling.
  try {
    // Do something that might throw an exception.
  } catch (e) {
    // Handle the exception.
  } on FormatException {
    // Handle a specific type of exception.
  } catch (e) {
    // Handle any other exception.
  } finally {
    // Always called, regardless of whether an exception was thrown or not.
  }

  // Assertions.
  assert(x > 0); // Throws an assertion error if x is not greater than 0.

  // JSON encoding.
  var jsonObject = {'name': 'Alice', 'age': 20};
  var jsonString = json.encode(jsonObject);

  // JSON decoding.
  var jsonString = '{"name": "Alice", "age": 20}';
  var jsonObject = json.decode(jsonString);
}
```

This code is a large and complex Dart program that demonstrates a variety of language features, including:

* Variables declaration and assignment
* Data types: integers, strings, booleans, and lists
* Operators and expressions
* Control statements: if, for, while, do-while, and switch
* Functions: regular, arrow, anonymous, and closures
* Higher-order functions
* List, set, and map comprehensions
* Generator functions
* Async functions
* Error handling: try-catch-finally and assertions
* JSON encoding and decoding

The code is heavily commented to explain what each part of the code does. It is also well-structured and uses a consistent coding style. Overall, this is a good example of a large and complex Dart program that demonstrates a variety of language features.