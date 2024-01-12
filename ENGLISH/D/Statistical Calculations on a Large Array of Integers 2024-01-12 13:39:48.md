```d
import std.stdio, std.algorithm, std.range, std.bigint;

void main() {
    // Define a large array of integers
    int[] largeArray = new int[1000000];

    // Fill the array with random values
    foreach (ref i; largeArray) {
        i = rand();
    }

    // Sort the array in ascending order
    largeArray.sort();

    // Find the maximum and minimum values in the array
    int maxValue = largeArray.max;
    int minValue = largeArray.min;

    // Calculate the sum of all the values in the array
    int sum = 0;
    foreach (i; largeArray) {
        sum += i;
    }

    // Calculate the average value in the array
    double average = sum / largeArray.length;

    // Find the median value in the array
    int median = largeArray[largeArray.length / 2];

    // Find the mode value in the array (the value that occurs most frequently)
    int mode = largeArray.mode();

    // Find the range of values in the array (the difference between the maximum and minimum values)
    int range = maxValue - minValue;

    // Find the variance of the values in the array (a measure of how spread out the values are)
    double variance = 0;
    foreach (i; largeArray) {
        variance += (i - average) * (i - average);
    }
    variance /= largeArray.length;

    // Find the standard deviation of the values in the array (a measure of how much the values deviate from the average)
    double standardDeviation = math.sqrt(variance);

    // Print the results to the console
    writeln("Maximum value:", maxValue);
    writeln("Minimum value:", minValue);
    writeln("Sum:", sum);
    writeln("Average:", average);
    writeln("Median:", median);
    writeln("Mode:", mode);
    writeln("Range:", range);
    writeln("Variance:", variance);
    writeln("Standard deviation:", standardDeviation);
}
```

This code performs a variety of statistical calculations on a large array of integers. It first fills the array with random values, then sorts it in ascending order. It then finds the maximum and minimum values, the sum of all the values, the average value, the median value, the mode value, the range of values, the variance of the values, and the standard deviation of the values. Finally, it prints the results to the console.

This code is complex because it performs a variety of different calculations on a large array of data. It also uses a number of advanced D language features, such as foreach loops, array slices, and the mode() method.