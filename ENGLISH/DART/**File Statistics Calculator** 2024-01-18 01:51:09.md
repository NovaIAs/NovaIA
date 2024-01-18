```dart
// Importing necessary libraries
import 'dart:io';
import 'dart:math';
import 'package:args/args.dart';

// Define command-line arguments
const argParser = ArgParser()
  ..addOption('input', abbr: 'i', help: 'Input file path')
  ..addOption('output', abbr: 'o', help: 'Output file path')
  ..addFlag('verbose', abbr: 'v', help: 'Enable verbose logging');

// Main function
void main(List<String> arguments) async {
  // Parse command-line arguments
  var results = argParser.parse(arguments);

  // Validate input and output file paths
  var inputFilePath = results['input'];
  if (inputFilePath == null) {
    print('Error: Input file path not provided.');
    print('Usage: dart script.dart -i <input_file_path> [-o <output_file_path>] [-v]');
    return;
  }
  var inputFile = File(inputFilePath);
  if (!inputFile.existsSync()) {
    print('Error: Input file does not exist.');
    return;
  }

  var outputFilePath = results['output'];
  var outputFile = File(outputFilePath ?? 'output.txt');

  // Enable verbose logging if specified
  var verbose = results['verbose'];

  // Read input file contents
  var inputLines = await inputFile.readAsLines();

  // Process input lines and calculate results
  var totalSum = 0;
  var primeNumbers = <int>[];
  for (var line in inputLines) {
    if (verbose) print('Processing line: $line');

    // Parse line
    var parts = line.split(',');
    var number1 = int.parse(parts[0]);
    var number2 = int.parse(parts[1]);

    // Calculate sum and check if any of the numbers is prime
    totalSum += number1 + number2;
    if (isPrime(number1)) primeNumbers.add(number1);
    if (isPrime(number2)) primeNumbers.add(number2);
  }

  // Generate output string
  var output = 'Total Sum: $totalSum\n';
  output += 'Prime Numbers Found: ${primeNumbers.join(', ')}\n';

  // Write output to file
  await outputFile.writeAsString(output);

  print('Results saved to: ${outputFile.path}');
}

// Function to check if a number is prime
bool isPrime(int number) {
  if (number <= 1) return false;
  for (var i = 2; i <= sqrt(number).toInt(); i++) {
    if (number % i == 0) return false;
  }
  return true;
}
```

**Explanation:**

This Dart program takes two input files, processes them line by line, and calculates various statistics about the numbers in each line. It then writes the results to an output file. Below is a detailed explanation:

1. **Command-Line Arguments:**
   - The program uses the `ArgParser` library to parse command-line arguments.
   - The `input` argument specifies the path to the input file.
   - The `output` argument (optional) specifies the path to the output file.
   - The `verbose` flag enables verbose logging.

2. **Input and Output File Validation:**
   - The program checks if the input file exists and is valid.
   - If the output file path is not specified, it uses the default name "output.txt".

3. **Processing Input Lines:**
   - The program reads the input file line by line.
   - For each line, it splits the line into two numbers using a comma (',') as the separator.
   - The program calculates the sum of the two numbers and checks if either of them is a prime number.

4. **Calculating Statistics:**
   - The program keeps track of the total sum of all numbers encountered.
   - It also maintains a list of prime numbers found in the input.

5. **Generating Output String:**
   - After processing all lines, the program generates an output string containing the total sum and a list of prime numbers found.

6. **Writing Results to File:**
   - The program writes the output string to the specified output file.

7. **isPrime Function:**
   - The `isPrime` function checks if a given number is prime.
   - It uses a simple algorithm to check for divisibility up to the square root of the number.

When you run this program with valid input and output file paths, it will process the input lines, calculate statistics, and save the results to the output file. The verbose logging (if enabled) will provide additional details about the processing of each line.