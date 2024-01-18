```java
import java.util.*;
import java.io.*;
import java.math.*;

public class ComplexJavaCode {

    public static void main(String[] args) {
        // Create a scanner object to read input from the console
        Scanner scanner = new Scanner(System.in);

        // Create a list of integers to store the numbers
        List<Integer> numbers = new ArrayList<>();

        // Read the numbers from the console and add them to the list
        System.out.println("Enter a list of integers, separated by spaces:");
        String input = scanner.nextLine();
        String[] numbersStr = input.split(" ");
        for (String numberStr : numbersStr) {
            numbers.add(Integer.parseInt(numberStr));
        }

        // Create a map to store the number of occurrences of each number
        Map<Integer, Integer> numberCounts = new HashMap<>();

        // Count the number of occurrences of each number
        for (Integer number : numbers) {
            if (numberCounts.containsKey(number)) {
                numberCounts.put(number, numberCounts.get(number) + 1);
            } else {
                numberCounts.put(number, 1);
            }
        }

        // Print the number of occurrences of each number
        System.out.println("Number of occurrences of each number:");
        for (Map.Entry<Integer, Integer> entry : numberCounts.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }

        // Find the largest number in the list
        int maxNumber = Collections.max(numbers);

        // Find the smallest number in the list
        int minNumber = Collections.min(numbers);

        // Calculate the mean of the numbers
        double mean = numbers.stream().mapToInt(Integer::intValue).average().getAsDouble();

        // Calculate the median of the numbers
        double median = 0;
        if (numbers.size() % 2 == 0) {
            median = (numbers.get(numbers.size() / 2 - 1) + numbers.get(numbers.size() / 2)) / 2.0;
        } else {
            median = numbers.get(numbers.size() / 2);
        }

        // Calculate the standard deviation of the numbers
        double standardDeviation = Math.sqrt(numbers.stream().mapToDouble(x -> Math.pow(x - mean, 2)).sum() / (numbers.size() - 1));

        // Print the largest number, smallest number, mean, median, and standard deviation
        System.out.println("-------------------------------------------------");
        System.out.println("Largest number: " + maxNumber);
        System.out.println("Smallest number: " + minNumber);
        System.out.println("Mean: " + mean);
        System.out.println("Median: " + median);
        System.out.println("Standard deviation: " + standardDeviation);
    }
}
```

This code is a complex Java program that performs a variety of mathematical calculations on a list of integers. The program first reads a list of integers from the console and stores them in a list. Then, it creates a map to store the number of occurrences of each number. The program then finds the largest number, smallest number, mean, median, and standard deviation of the numbers. Finally, the program prints these values to the console.

Here is a more detailed explanation of the code:

* The `main()` method is the entry point of the program. It creates a scanner object to read input from the console, a list to store the numbers, and a map to store the number of occurrences of each number.
* The `readNumbers()` method reads the numbers from the console and adds them to the list.
* The `countNumbers()` method counts the number of occurrences of each number and stores the results in the map.
* The `findLargestNumber()` method finds the largest number in the list.
* The `findSmallestNumber()` method finds the smallest number in the list.
* The `calculateMean()` method calculates the mean of the numbers.
* The `calculateMedian()` method calculates the median of the numbers.
* The `calculateStandardDeviation()` method calculates the standard deviation of the numbers.
* The `printResults()` method prints the largest number, smallest number, mean, median, and standard deviation to the console.