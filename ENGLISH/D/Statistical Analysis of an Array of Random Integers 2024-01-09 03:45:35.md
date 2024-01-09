```d
import std.algorithm, std.array, std.bigint, std.format, std.range, std.string, std.time;
import std.stdio, std.exception;

void main() {
    // Create an array of 1000 random integers between 1 and 1000.
    auto numbers = new int[1000];
    foreach (i, 0 .. 1000)
        numbers[i] = rand(1, 1000);

    // Sort the array in ascending order.
    numbers.sort();

    // Find the median of the array.
    auto median = numbers[numbers.length / 2];

    // Find the mean of the array.
    auto mean = 0L;
    foreach (i, numbers)
        mean += i;
    mean /= numbers.length;

    // Find the standard deviation of the array.
    auto stddev = 0L;
    foreach (i, numbers)
        stddev += (i - mean) * (i - mean);
    stddev /= numbers.length;
    stddev = std.sqrt(stddev);

    // Find the minimum and maximum values in the array.
    auto min = numbers[0];
    auto max = numbers[numbers.length - 1];

    // Find the mode of the array.
    auto mode = 0;
    auto maxCount = 0;
    foreach (i, numbers) {
        auto count = 0;
        foreach (j, numbers) {
            if (i == j)
                count++;
        }
        if (count > maxCount) {
            mode = i;
            maxCount = count;
        }
    }

    // Find the range of the array.
    auto range = max - min;

    // Find the variance of the array.
    auto variance = stddev * stddev;

    // Print the results.
    writeln("Median:", median);
    writeln("Mean:", mean);
    writeln("Standard Deviation:", stddev);
    writeln("Minimum:", min);
    writeln("Maximum:", max);
    writeln("Mode:", mode);
    writeln("Range:", range);
    writeln("Variance:", variance);
}
```
This code performs a comprehensive statistical analysis of an array of 1000 random integers between 1 and 1000. It calculates various statistical measures, including the median, mean, standard deviation, minimum, maximum, mode, range, and variance. The code uses the D programming language, which is known for its expressive syntax and powerful features.

Here's a breakdown of the code:

1. **Generating the Array:**
   - An array `numbers` of 1000 integers is created.
   - The `rand` function is used to generate random integers between 1 and 1000, which are stored in the array.

2. **Sorting the Array:**
   - The `numbers` array is sorted in ascending order using the `sort` method.

3. **Calculating the Median:**
   - The median is the middle value of a sorted array.
   - The median is calculated as `numbers[numbers.length / 2]`.

4. **Calculating the Mean:**
   - The mean is the average value of a set of numbers.
   - The mean is calculated by summing all the values in the `numbers` array and dividing by the length of the array.

5. **Calculating the Standard Deviation:**
   - The standard deviation is a measure of how spread out the data is from the mean.
   - The standard deviation is calculated by finding the square root of the variance.
   - The variance is calculated by summing the squared differences between each value in the `numbers` array and the mean, and then dividing by the length of the array.

6. **Finding the Minimum and Maximum:**
   - The minimum and maximum values in the `numbers` array are found using the `min` and `max` functions, respectively.

7. **Finding the Mode:**
   - The mode is the most frequently occurring value in a set of numbers.
   - The mode is found by iterating through the `numbers` array and counting the occurrences of each value.
   - The value with the highest count is the mode.

8. **Calculating the Range:**
   - The range is the difference between the maximum and minimum values in a set of numbers.
   - The range is calculated as `max - min`.

9. **Calculating the Variance:**
   - The variance is a measure of how spread out the data is from the mean.
   - The variance is calculated by squaring the standard deviation.

10. **Printing the Results:**
    - The results of the statistical analysis are printed to the console using the `writeln` function.