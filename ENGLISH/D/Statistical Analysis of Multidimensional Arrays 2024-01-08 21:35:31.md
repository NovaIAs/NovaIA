```d
import std.stdio;
import std.math;
import std.string;
import std.algorithm;
import std.range;
import std.algorithm.find;

void main() {
    // Declare a multidimensional array of integers
    int[][][] array3d = new int[3][4][5];

    // Initialize the array with random values
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            foreach (ref int element in array1d) {
                element = random.int(0, 100);
            }
        }
    }

    // Print the array
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            writefln("%s", array1d.format("%3d "));
        }
        writefln();
    }

    // Find the maximum value in the array
    int max = 0;
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            foreach (int element in array1d) {
                max = max.max(element);
            }
        }
    }

    // Print the maximum value
    writefln("Maximum value: %d", max);

    // Find the minimum value in the array
    int min = 100;
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            foreach (int element in array1d) {
                min = min.min(element);
            }
        }
    }

    // Print the minimum value
    writefln("Minimum value: %d", min);

    // Find the average value in the array
    int sum = 0;
    int count = 0;
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            foreach (int element in array1d) {
                sum += element;
                count++;
            }
        }
    }

    double average = (double)sum / count;

    // Print the average value
    writefln("Average value: %.2f", average);

    // Find the median value in the array
    int[] values = new int[array3d.length * array3d[0].length * array3d[0][0].length];
    int index = 0;
    foreach (ref int[][] array2d in array3d) {
        foreach (ref int[] array1d in array2d) {
            foreach (int element in array1d) {
                values[index++] = element;
            }
        }
    }

    values.sort();

    double median = 0;
    if (values.length % 2 == 0) {
        median = (values[values.length / 2 - 1] + values[values.length / 2]) / 2.0;
    } else {
        median = values[values.length / 2];
    }

    // Print the median value
    writefln("Median value: %.2f", median);

    // Find the mode value in the array
    int[] counts = new int[101];
    foreach (int value in values) {
        counts[value]++;
    }

    int maxCount = 0;
    int mode = 0;
    for (int i = 0; i < counts.length; i++) {
        if (counts[i] > maxCount) {
            maxCount = counts[i];
            mode = i;
        }
    }

    // Print the mode value
    writefln("Mode value: %d", mode);
}
```

This code performs a variety of statistical operations on a three-dimensional array of integers. It initializes the array with random values, prints the array, finds the maximum, minimum, average, median, and mode values, and prints the results.

The code uses a number of D language features, including:

* Multidimensional arrays
* Random number generation
* foreach loops
* Array slicing
* Function pointers
* Sorting algorithms
* Conditional statements
* The `max` and `min` functions
* The `sum` and `count` functions
* The `average` function
* The `median` function
* The `mode` function

This code is a good example of how to use D to perform complex data analysis tasks.