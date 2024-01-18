```dart
import 'dart:io';
import 'dart:convert';

Future<void> main() async {
  // Get the current working directory.
  var cwd = Directory.current.path;

  // Read the contents of the 'input.txt' file.
  var inputFile = File('$cwd/input.txt');
  var inputText = await inputFile.readAsString();

  // Parse the input text into a list of integers.
  var numbers = inputText.split('\n').map(int.parse).toList();

  // Sort the list of integers in ascending order.
  numbers.sort((a, b) => a.compareTo(b));

  // Write the sorted list of integers to the 'output.txt' file.
  var outputFile = File('$cwd/output.txt');
  await outputFile.writeAsString(numbers.join('\n'));

  // Print a message to the console.
  print('The sorted list of integers has been written to '
      'the "output.txt" file.');
}
```

This code reads a list of integers from a file named 'input.txt', sorts the list in ascending order, and then writes the sorted list to a file named 'output.txt'.

The code first imports the necessary libraries and defines a main function. The main function is where the execution of the program begins.

The first thing the main function does is get the current working directory using the Directory.current.path property. This is done so that the program can access the 'input.txt' and 'output.txt' files, which are located in the current working directory.

Next, the main function reads the contents of the 'input.txt' file using the File.readAsString() method. The readAsString() method returns a Future<String>, which is a promise to deliver a string value at some point in the future. The await keyword is used to wait for the Future to complete and obtain the string value.

Once the input text has been read, it is parsed into a list of integers using the split() and map() methods. The split() method splits the input text into a list of strings, where each string represents an integer. The map() method then converts each string to an integer using the int.parse() method.

The resulting list of integers is then sorted in ascending order using the sort() method. The sort() method takes a comparator function as an argument. The comparator function compares two integers and returns a value that indicates which integer is greater. In this case, the comparator function is (a, b) => a.compareTo(b), which simply compares the two integers using the compareTo() method.

Finally, the sorted list of integers is written to the 'output.txt' file using the File.writeAsString() method. The writeAsString() method takes a string as an argument and writes it to the file.

The main function also prints a message to the console indicating that the sorted list of integers has been written to the 'output.txt' file.