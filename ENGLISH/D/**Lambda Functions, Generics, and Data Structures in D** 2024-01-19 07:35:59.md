```d
import std.stdio, std.string, std.algorithm;

void main() {
    // Create a lambda function that squares a number
    auto square = [](int x) { return x * x; };

    // Create a vector of numbers from 1 to 10
    auto numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Map the square lambda over the numbers vector, squaring each number
    auto squaredNumbers = numbers.map!(square);

    // Print the squared numbers
    writefln("Squared numbers: %s", squaredNumbers.to!string);

    // Find the maximum squared number
    auto maxSquaredNumber = squaredNumbers.reduce!(max, 0);

    // Print the maximum squared number
    writefln("Maximum squared number: %d", maxSquaredNumber);

    // Sort the squared numbers in ascending order
    squaredNumbers.sort!();

    // Print the sorted squared numbers
    writefln("Sorted squared numbers: %s", squaredNumbers.to!string);

    // Create a dictionary of numbers mapped to their squares
    auto numberToSquare = new HashMap!int, int;
    for (int i = 0; i < numbers.length; i++) {
        numberToSquare[numbers[i]] = squaredNumbers[i];
    }

    // Print the number to square dictionary
    writefln("Number to square dictionary:");
    for (key, value in numberToSquare) {
        writefln("    %d: %d", key, value);
    }
}
```

This code demonstrates various language features of D, including lambda functions, generics, function mapping, function reduction, sorting, dictionaries, and loops. It creates a vector of numbers, squares each number using a lambda function, finds the maximum squared number, sorts the squared numbers, and creates a dictionary of numbers mapped to their squares. It then prints out the results of each step.

Here's a detailed explanation of the code:

1. Define a lambda function named `square` that takes an integer `x` as input and returns its square (`x * x`).
2. Create a vector `numbers` containing the integers from 1 to 10 using a range expression `[1, 2, 3, ..., 10]`.
3. Use the `map!` function to apply the `square` lambda to each element of the `numbers` vector, resulting in a new vector `squaredNumbers` containing the squares of each number.
4. Find the maximum squared number in the `squaredNumbers` vector using the `reduce!` function with the `max` function as the reduction operation and an initial value of 0.
5. Sort the `squaredNumbers` vector in ascending order using the `sort!` function.
6. Create a new HashMap `numberToSquare` that maps each number in the `numbers` vector to its corresponding squared number in the `squaredNumbers` vector.
7. Iterate through the `numberToSquare` dictionary using a `for` loop and print out the key-value pairs.

This code showcases the flexibility and expressiveness of D, allowing you to write complex code in a concise and readable manner.