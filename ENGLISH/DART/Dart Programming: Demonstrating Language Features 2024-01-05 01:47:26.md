```dart
// Import the necessary libraries.
import 'dart:async';
import 'dart:math';
import 'dart:io';

// Define the main function.
void main() async {
  // Create a new Random object.
  Random random = new Random();

  // Create a new List of integers.
  List<int> numbers = new List<int>();

  // Populate the List with random integers.
  for (int i = 0; i < 100; i++) {
    numbers.add(random.nextInt(100));
  }

  // Sort the List in ascending order.
  numbers.sort();

  // Print the sorted List to the console.
  print(numbers);

  // Create a new File object.
  File file = new File('numbers.txt');

  // Open the File for writing.
  IOSink sink = file.openWrite();

  // Write the sorted List to the File.
  for (int number in numbers) {
    sink.write('$number\n');
  }

  // Close the File.
  sink.close();

  // Create a new StreamController.
  StreamController<int> controller = new StreamController<int>();

  // Create a new Stream from the StreamController.
  Stream<int> stream = controller.stream;

  // Subscribe to the Stream.
  stream.listen((int number) {
    print('Received $number');
  });

  // Add some integers to the StreamController.
  for (int i = 0; i < 10; i++) {
    controller.add(i);
  }

  // Close the StreamController.
  controller.close();

  // Create a new Future.
  Future<int> future = new Future<int>((Completer<int> completer) {
    // Simulate an asynchronous operation.
    new Timer(new Duration(seconds: 5), () {
      // Complete the Future with the result.
      completer.complete(100);
    });
  });

  // Wait for the Future to complete.
  int result = await future;

  // Print the result to the console.
  print('The result is $result');
}
```

This code demonstrates a variety of features of the Dart programming language, including:

* **Random number generation:** The `Random` class is used to generate random integers.
* **List manipulation:** The `List` class is used to store and manipulate a collection of integers.
* **Sorting:** The `sort()` method is used to sort the List in ascending order.
* **File I/O:** The `File` class is used to create and write to a file.
* **Streams:** The `StreamController` and `Stream` classes are used to create and subscribe to a stream of data.
* **Futures:** The `Future` class is used to represent an asynchronous operation.
* **Error handling:** The `try-catch` statement is used to handle errors.
* **Asynchronous programming:** The `await` keyword is used to wait for a Future to complete.

This code is a good example of how to use Dart to write complex and efficient programs.