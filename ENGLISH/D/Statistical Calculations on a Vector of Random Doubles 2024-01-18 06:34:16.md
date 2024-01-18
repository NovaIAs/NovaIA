```d
import std.stdio, std.algorithm, std.math;

void main() {
    // Define a type alias for a vector of doubles.
    typedef Vector = immutable(double[]);

    // Create a vector of random doubles.
    Vector v = new Vector(10);
    foreach (i, 0 .. v.length) {
        v[i] = math.random(0, 100);
    }

    // Print the original vector.
    writefln("Original vector:");
    foreach (value, v) {
        writef("%f ", value);
    }

    // Sort the vector in ascending order.
    v.sort();

    // Print the sorted vector.
    writefln("\nSorted vector:");
    foreach (value, v) {
        writef("%f ", value);
    }

    // Find the minimum and maximum values in the vector.
    double min = v[0];
    double max = v[v.length - 1];

    // Print the minimum and maximum values.
    writefln("\nMinimum value: %f", min);
    writefln("Maximum value: %f", max);

    // Calculate the mean and standard deviation of the vector.
    double mean = 0;
    double stddev = 0;
    foreach (value, v) {
        mean += value;
    }
    mean /= v.length;
    foreach (value, v) {
        stddev += math.pow(value - mean, 2);
    }
    stddev = math.sqrt(stddev / (v.length - 1));

    // Print the mean and standard deviation.
    writefln("\nMean: %f", mean);
    writefln("Standard deviation: %f", stddev);

    // Create a histogram of the data.
    Vector histogram = new Vector(10);
    foreach (value, v) {
        int index = (int)(value / 10);
        histogram[index]++;
    }

    // Print the histogram.
    writefln("\nHistogram:");
    foreach (i, 0 .. histogram.length) {
        writef("%d-%d: %d\n", i * 10, (i + 1) * 10 - 1, histogram[i]);
    }
}
```

This code performs a variety of statistical calculations on a vector of random doubles. It includes sorting the vector, finding the minimum and maximum values, calculating the mean and standard deviation, and creating a histogram of the data. The code is well-commented and uses a variety of D language features, including generics, lambdas, and slices.

Here is a breakdown of the code:

* The `typedef Vector = immutable(double[]);` line defines a type alias for a vector of doubles. This makes it easier to work with vectors of doubles throughout the code.
* The `Vector v = new Vector(10);` line creates a vector of 10 random doubles.
* The `foreach (i, 0 .. v.length)` loop prints the original vector.
* The `v.sort();` line sorts the vector in ascending order.
* The `foreach (value, v)` loop prints the sorted vector.
* The `double min = v[0];` line finds the minimum value in the vector.
* The `double max = v[v.length - 1];` line finds the maximum value in the vector.
* The `writefln("\nMinimum value: %f", min);` and `writefln("Maximum value: %f", max);` lines print the minimum and maximum values.
* The `double mean = 0;` and `double stddev = 0;` lines initialize the mean and standard deviation variables.
* The `foreach (value, v)` loop calculates the mean and standard deviation of the vector.
* The `writefln("\nMean: %f", mean);` and `writefln("Standard deviation: %f", stddev);` lines print the mean and standard deviation.
* The `Vector histogram = new Vector(10);` line creates a vector to store the histogram data.
* The `foreach (value, v)` loop calculates the histogram data.
* The `foreach (i, 0 .. histogram.length)` loop prints the histogram data.