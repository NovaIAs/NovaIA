```d
import std.stdio, std.algorithm, std.string, std.random;

void main() {
    // Create an array of 100 random integers
    int[] randomArray = new int[100];
    foreach (i, randomArray) {
        randomArray[i] = Random.int(100);
    }

    // Sort the array in ascending order
    Algorithm.sort(randomArray);

    // Create a string containing the sorted array
    string sortedArrayString = "";
    foreach (i, randomArray) {
        sortedArrayString += randomArray[i].string();
        if (i < randomArray.length - 1) {
            sortedArrayString += ", ";
        }
    }

    // Print the sorted array to the console
    writefln("Sorted array: %s", sortedArrayString);

    // Find the largest and smallest values in the array
    int max = randomArray[0];
    int min = randomArray[0];
    foreach (i, randomArray) {
        if (randomArray[i] > max) {
            max = randomArray[i];
        }
        if (randomArray[i] < min) {
            min = randomArray[i];
        }
    }

    // Print the largest and smallest values to the console
    writefln("Maximum value: %d", max);
    writefln("Minimum value: %d", min);

    // Create a frequency table to count the occurrences of each value in the array
    HashMap<int, int> frequencyTable = new HashMap<int, int>();
    foreach (i, randomArray) {
        if (frequencyTable.containsKey(randomArray[i])) {
            frequencyTable[randomArray[i]]++;
        } else {
            frequencyTable[randomArray[i]] = 1;
        }
    }

    // Print the frequency table to the console
    foreach (key, value, frequencyTable) {
        writefln("Value %d occurs %d times", key, value);
    }
}
```

This code generates an array of 100 random integers, sorts it in ascending order, finds the largest and smallest values in the array, and creates a frequency table to count the occurrences of each value in the array. It then prints the sorted array, the largest and smallest values, and the frequency table to the console.

Here's a breakdown of the code:

* The `import` statement imports the necessary modules.
* The `main` function is the entry point of the program.
* The `int[] randomArray = new int[100];` line creates an array of 100 integers.
* The `foreach (i, randomArray)` loop iterates over each element of the array and assigns it a random integer between 0 and 99.
* The `Algorithm.sort(randomArray);` line sorts the array in ascending order.
* The `string sortedArrayString = "";` line creates a string to store the sorted array.
* The `foreach (i, randomArray)` loop iterates over each element of the array and appends it to the `sortedArrayString` string.
* The `writefln("Sorted array: %s", sortedArrayString);` line prints the sorted array to the console.
* The `int max = randomArray[0];` and `int min = randomArray[0];` lines initialize the `max` and `min` variables to the first element of the array.
* The `foreach (i, randomArray)` loop iterates over each element of the array and updates the `max` and `min` variables if a larger or smaller value is found.
* The `writefln("Maximum value: %d", max);` and `writefln("Minimum value: %d", min);` lines print the largest and smallest values to the console.
* The `HashMap<int, int> frequencyTable = new HashMap<int, int>();` line creates a hashmap to store the frequency of each value in the array.
* The `foreach (i, randomArray)` loop iterates over each element of the array and updates the frequency of the value in the hashmap.
* The `foreach (key, value, frequencyTable)` loop iterates over each key-value pair in the hashmap and prints the value and its frequency to the console.