```java
import java.util.*;
import java.lang.*;

public class ComplexJavaCode {

    private static final String[] COLORS = {"red", "orange", "yellow", "green", "blue", "indigo", "violet"};

    public static void main(String[] args) {
        // Create a list of fruits
        List<String> fruits = new ArrayList<>();
        fruits.add("apple");
        fruits.add("banana");
        fruits.add("cherry");
        fruits.add("durian");
        fruits.add("elderberry");

        // Create a map of colors to their corresponding wavelengths
        Map<String, Integer> colorWavelengths = new HashMap<>();
        colorWavelengths.put("red", 620);
        colorWavelengths.put("orange", 590);
        colorWavelengths.put("yellow", 570);
        colorWavelengths.put("green", 520);
        colorWavelengths.put("blue", 480);
        colorWavelengths.put("indigo", 450);
        colorWavelengths.put("violet", 400);

        // Create a set of numbers
        Set<Integer> numbers = new HashSet<>();
        numbers.add(1);
        numbers.add(2);
        numbers.add(3);
        numbers.add(4);
        numbers.add(5);

        // Print the fruits, colors, and numbers to the console
        System.out.println("Fruits:");
        for (String fruit : fruits) {
            System.out.println(fruit);
        }

        System.out.println("Colors and their corresponding wavelengths:");
        for (Map.Entry<String, Integer> entry : colorWavelengths.entrySet()) {
            System.out.println(entry.getKey() + ":\t" + entry.getValue());
        }

        System.out.println("Numbers:");
        for (Integer number : numbers) {
            System.out.println(number);
        }

        // Find the longest fruit and the shortest color
        String longestFruit = "";
        String shortestColor = "";
        for (String fruit : fruits) {
            if (fruit.length() > longestFruit.length()) {
                longestFruit = fruit;
            }
        }
        for (String color : COLORS) {
            if (color.length() < shortestColor.length() || shortestColor.isEmpty()) {
                shortestColor = color;
            }
        }

        // Print the longest fruit and the shortest color to the console
        System.out.println("Longest fruit:\t" + longestFruit);
        System.out.println("Shortest color:\t" + shortestColor);

        // Find the sum of the numbers in the set
        int sum = 0;
        for (Integer number : numbers) {
            sum += number;
        }

        // Print the sum of the numbers to the console
        System.out.println("Sum of the numbers:\t" + sum);

        // Find the average wavelength of the colors in the map
        double averageWavelength = 0;
        for (Map.Entry<String, Integer> entry : colorWavelengths.entrySet()) {
            averageWavelength += entry.getValue();
        }
        averageWavelength /= colorWavelengths.size();

        // Print the average wavelength to the console
        System.out.println("Average wavelength:\t" + averageWavelength);

        // Find the median number in the set
        List<Integer> sortedNumbers = new ArrayList<>(numbers);
        Collections.sort(sortedNumbers);
        int medianNumber;
        if (sortedNumbers.size() % 2 == 0) {
            medianNumber = (sortedNumbers.get(sortedNumbers.size() / 2) + sortedNumbers.get(sortedNumbers.size() / 2 - 1)) / 2;
        } else {
            medianNumber = sortedNumbers.get(sortedNumbers.size() / 2);
        }

        // Print the median number to the console
        System.out.println("Median number:\t" + medianNumber);
    }
}
```

This code is a complex Java program that performs a variety of tasks, including:

* Creating and printing a list of fruits
* Creating and printing a map of colors to their corresponding wavelengths
* Creating and printing a set of numbers
* Finding and printing the longest fruit and the shortest color
* Finding and printing the sum of the numbers in the set
* Finding and printing the average wavelength of the colors in the map
* Finding and printing the median number in the set

The code is well-commented and uses a variety of Java features, including lists, maps, sets, and loops. It also demonstrates the use of conditional statements and mathematical operations.