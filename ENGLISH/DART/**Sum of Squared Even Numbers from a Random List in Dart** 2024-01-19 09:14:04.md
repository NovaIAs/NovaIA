```dart
// Import required libraries
import 'dart:async';
import 'dart:math';

// Define the main function
void main() async {
  // Create a list of 100 random numbers
  List<int> numbers = [];
  for (int i = 0; i < 100; i++) {
    numbers.add(Random().nextInt(100));
  }

  // Print the original list
  print("Original List: $numbers");

  // Create a stream of numbers from the list
  Stream<int> numbersStream = Stream.fromIterable(numbers);

  // Filter out even numbers using the where() method
  Stream<int> evenNumbersStream = numbersStream.where((number) => number % 2 == 0);

  // Map each number to its square using the map() method
  Stream<int> squaredNumbersStream = evenNumbersStream.map((number) => number * number);

  // Sum up all the squared numbers using the reduce() method
  int sumOfSquaredEvenNumbers = await squaredNumbersStream.reduce((a, b) => a + b);

  // Print the sum of the squared even numbers
  print("Sum of Squared Even Numbers: $sumOfSquaredEvenNumbers");
}
```

**Explanation:**

1. **Generating a List of Random Numbers:**
   - We create a list `numbers` and fill it with 100 random integers using a `for` loop.

2. **Creating a Stream of Numbers:**
   - We use `Stream.fromIterable()` to convert the `numbers` list into a stream of integers.

3. **Filtering Even Numbers:**
   - We use the `where()` method on the stream to filter out even numbers. This means that only even numbers will be passed on to the next step.

4. **Mapping Numbers to Their Squares:**
   - We use the `map()` method on the filtered stream to square each even number.

5. **Summing Up Squared Numbers:**
   - We use the `reduce()` method on the squared numbers stream to sum up all the values. This results in a single value, which is the sum of all the squared even numbers in the original list.

6. **Printing the Result:**
   - Finally, we print the sum of the squared even numbers.

This code demonstrates several important concepts in Dart:

- **Streams:** Streams are a powerful way to handle asynchronous data in Dart. They allow us to process data as it becomes available, rather than waiting for the entire dataset to be collected.
- **Stream Transformations:** Streams can be transformed in various ways using methods like `where()`, `map()`, and `reduce()`. These transformations allow us to manipulate the data in the stream and extract the information we need.
- **Asynchronous Programming:** The `await` keyword is used to wait for asynchronous operations to complete. In this case, we use `await` with the `reduce()` method to ensure that the sum of squared even numbers is calculated correctly before printing the result.