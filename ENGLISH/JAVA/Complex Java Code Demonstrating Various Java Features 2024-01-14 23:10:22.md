```java
import java.util.*;
import java.io.*;
import java.math.*;

public class ComplexJavaCode {

    // Main method
    public static void main(String[] args) {
        // Initialize variables
        int[] array = {1, 2, 3, 4, 5};
        String[] strings = {"Hello", "World", "!"};
        Map<String, Integer> map = new HashMap<>();
        Set<Integer> set = new HashSet<>();

        // Iterate over the array and print each element
        for (int element : array) {
            System.out.println(element);
        }

        // Iterate over the strings array and print each element
        for (String string : strings) {
            System.out.println(string);
        }

        // Add key-value pairs to the map
        map.put("One", 1);
        map.put("Two", 2);
        map.put("Three", 3);

        // Iterate over the map and print each key-value pair
        for (Map.Entry<String, Integer> entry : map.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }

        // Add elements to the set
        set.add(1);
        set.add(2);
        set.add(3);

        // Iterate over the set and print each element
        for (Integer element : set) {
            System.out.println(element);
        }

        // Create a new list and add elements to it
        List<Integer> list = new ArrayList<>();
        list.add(1);
        list.add(2);
        list.add(3);

        // Iterate over the list and print each element
        for (Integer element : list) {
            System.out.println(element);
        }

        // Create a new BigDecimal object
        BigDecimal bigDecimal = new BigDecimal("1.23456789");

        // Print the BigDecimal object
        System.out.println(bigDecimal);

        // Create a new BigInteger object
        BigInteger bigInteger = new BigInteger("123456789");

        // Print the BigInteger object
        System.out.println(bigInteger);

        // Create a new Date object
        Date date = new Date();

        // Print the Date object
        System.out.println(date);

        // Create a new File object
        File file = new File("file.txt");

        // Check if the file exists
        if (file.exists()) {
            System.out.println("File exists");
        } else {
            System.out.println("File does not exist");
        }

        // Create a new Scanner object to read from the console
        Scanner scanner = new Scanner(System.in);

        // Read a line of input from the console
        String input = scanner.nextLine();

        // Print the input
        System.out.println(input);

        // Close the Scanner object
        scanner.close();
    }
}
```

This code is a complex Java program that demonstrates a variety of Java features, including arrays, strings, maps, sets, lists, BigDecimals, BigIntegers, dates, files, and input/output. The code is well-organized and uses comments to explain each section. The code is also efficient and uses the latest Java features.