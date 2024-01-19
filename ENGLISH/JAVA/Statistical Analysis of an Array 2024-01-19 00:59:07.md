```java
import java.util.*;
import java.math.*;

public class ExtremelyComplexCode {

    private static final int MAX_VALUE = 1000000;
    private static final int MIN_VALUE = -1000000;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Read input
        int n = scanner.nextInt();
        int[] array = new int[n];
        for (int i = 0; i < n; i++) {
            array[i] = scanner.nextInt();
        }

        // Find the maximum and minimum values in the array
        int max = MAX_VALUE;
        int min = MIN_VALUE;
        for (int i = 0; i < n; i++) {
            max = Math.max(max, array[i]);
            min = Math.min(min, array[i]);
        }

        // Calculate the range of the array
        int range = max - min;

        // Find the frequency of each element in the array
        Map<Integer, Integer> frequencyMap = new HashMap<Integer, Integer>();
        for (int i = 0; i < n; i++) {
            int element = array[i];
            if (frequencyMap.containsKey(element)) {
                frequencyMap.put(element, frequencyMap.get(element) + 1);
            } else {
                frequencyMap.put(element, 1);
            }
        }

        // Find the mode of the array
        int mode = 0;
        int maxFrequency = 0;
        for (Map.Entry<Integer, Integer> entry : frequencyMap.entrySet()) {
            int element = entry.getKey();
            int frequency = entry.getValue();
            if (frequency > maxFrequency) {
                mode = element;
                maxFrequency = frequency;
            }
        }

        // Find the mean of the array
        int sum = 0;
        for (int i = 0; i < n; i++) {
            sum += array[i];
        }
        double mean = (double) sum / n;

        // Find the median of the array
        Arrays.sort(array);
        int median = 0;
        if (n % 2 == 0) {
            median = (array[n / 2] + array[n / 2 - 1]) / 2;
        } else {
            median = array[n / 2];
        }

        // Print the results
        System.out.println("Maximum value: " + max);
        System.out.println("Minimum value: " + min);
        System.out.println("Range: " + range);
        System.out.println("Mode: " + mode);
        System.out.println("Mean: " + mean);
        System.out.println("Median: " + median);
    }
}
```

This code is a complex Java program that performs various statistical calculations on an array of integers. It reads input from the user, finds the maximum and minimum values in the array, calculates the range of the array, finds the frequency of each element in the array, finds the mode of the array, finds the mean of the array, and finds the median of the array. The program then prints the results to the user.

Here is a detailed explanation of the code:

* **Import Statements:**

```java
import java.util.*;
import java.math.*;
```

These import statements import the necessary Java libraries. The `java.util` library contains classes and interfaces for working with collections, such as arrays and lists. The `java.math` library contains classes and interfaces for performing mathematical operations on large numbers.

* **Constants:**

```java
private static final int MAX_VALUE = 1000000;
private static final int MIN_VALUE = -1000000;
```

These constants define the maximum and minimum values that can be stored in the array.

* **Main Method:**

```java
public static void main(String[] args) {
    // ...
}
```

The `main` method is the entry point of the program. It reads input from the user, performs the statistical calculations, and prints the results.

* **Read Input:**

```java
int n = scanner.nextInt();
int[] array = new int[n];
for (int i = 0; i < n; i++) {
    array[i] = scanner.nextInt();
}
```

These lines of code read input from the user. The first line reads the number of elements in the array. The second line creates an array of integers with the specified size. The third line reads the elements of the array from the user.

* **Find the Maximum and Minimum Values:**

```java
int max = MAX_VALUE;
int min = MIN_VALUE;
for (int i = 0; i < n; i++) {
    max = Math.max(max, array[i]);
    min = Math.min(min, array[i]);
}
```

These lines of code find the maximum and minimum values in the array. The `Math.max()` and `Math.min()` methods are used to compare the current value of `max` and `min` with the current element in the array.

* **Calculate the Range:**

```java
int range = max - min;
```

The range of the array is simply the difference between the maximum and minimum values.

* **Find the Frequency of Each Element:**

```java
Map<Integer, Integer> frequencyMap = new HashMap<Integer, Integer>();
for (int i = 0; i < n; i++) {
    int element = array[i];
    if (frequencyMap.containsKey(element)) {
        frequencyMap.put(element, frequencyMap.get(element) + 1);
    } else {
        frequencyMap.put(element, 1);
    }
}
```

These lines of code find the frequency of each element in the array. The `HashMap` is used to store the frequency of each element. The `containsKey()` method is used to check if the element is already in the map. If it is, the frequency is incremented by 1. If it is not, the element is added to the map with a frequency of 1.

* **Find the Mode:**

```java
int mode = 0;
int maxFrequency = 0;
for (Map.Entry<Integer, Integer> entry : frequencyMap.entrySet()) {
    int element = entry.getKey();
    int frequency = entry.getValue();
    if (frequency > maxFrequency) {
        mode = element;
        maxFrequency = frequency;
    }
}
```

These lines of code find the mode of the array. The mode is the most frequently occurring element in the array. The `entrySet()` method is used to iterate over the entries in the frequency map. For each entry, the element and frequency are retrieved. The `if` statement checks if the current frequency is greater than the current maximum frequency. If it is, the mode is updated to the current element and the maximum frequency is updated to the current frequency.

* **Find the Mean:**

```java
int sum = 0;
for (int i = 0; i < n; i++) {
    sum += array[i];
}
double mean = (double) sum / n;
```

These lines of code find the mean of the array. The mean is the average value of the elements in the array. The `sum` variable is used to store the sum of the elements in the array. The `mean` variable is calculated by dividing the sum by the number of elements in the array.

* **Find the Median:**

```java
Arrays.sort(array);
int median = 0;
if (n % 2 == 0) {
    median = (array[n / 2] + array[n / 2 - 1]) / 2;
} else {
    median = array[n / 2];
}
```

These lines of code find the median of the array. The median is the middle value of the array when the array is sorted in ascending order. The `Arrays.sort()` method is used to sort the array in ascending order. The `if` statement checks if the number of elements in the array is even or odd. If it is even, the median is calculated as the average of the two middle elements. If it is odd, the median is calculated as the middle element.

* **Print the Results:**

```java
System.out.println("Maximum value: " + max);
System.out.println("Minimum value: " + min);
System.out.println("Range: " + range);
System.out.println("Mode: " + mode);
System.out.println("Mean: " + mean);
System.out.println("Median: " + median);
```

These lines of code print the results to the user. The `System.out.println()` method is used to print the results to the console.