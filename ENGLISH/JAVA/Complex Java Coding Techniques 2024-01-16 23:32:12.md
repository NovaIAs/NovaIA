```java
import java.util.*;
import java.io.*;
import java.math.*;
import java.text.*;

public class ComplexJavaCode {

    // Define a custom exception
    static class CustomException extends Exception {
        public CustomException(String message) {
            super(message);
        }
    }

    // Define a generic method to find the maximum of two elements
    static <T extends Comparable<T>> T max(T a, T b) {
        return a.compareTo(b) > 0 ? a : b;
    }

    // Define a recursive method to calculate the factorial of a number
    static int factorial(int n) {
        return n == 0 ? 1 : n * factorial(n - 1);
    }

    // Define a method to check if a number is prime
    static boolean isPrime(int n) {
        if (n <= 1) {
            return false;
        }
        for (int i = 2; i <= Math.sqrt(n); i++) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }

    // Define a method to find the greatest common divisor of two numbers
    static int gcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }

    // Define a method to find the least common multiple of two numbers
    static int lcm(int a, int b) {
        return a * b / gcd(a, b);
    }

    // Define a method to generate a random number between two numbers
    static int random(int min, int max) {
        return (int) (Math.random() * (max - min + 1) + min);
    }

    // Define a method to sort an array of integers in ascending order
    static void sort(int[] arr) {
        Arrays.sort(arr);
    }

    // Define a method to reverse an array of integers
    static void reverse(int[] arr) {
        for (int i = 0, j = arr.length - 1; i < j; i++, j--) {
            int temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
        }
    }

    // Define a method to search for an element in an array of integers
    static int search(int[] arr, int target) {
        for (int i = 0; i < arr.length; i++) {
            if (arr[i] == target) {
                return i;
            }
        }
        return -1;
    }

    // Define a method to print an array of integers
    static void print(int[] arr) {
        for (int i = 0; i < arr.length; i++) {
            System.out.print(arr[i] + " ");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        try {
            // Throw a custom exception
            throw new CustomException("This is a custom exception.");
        } catch (CustomException e) {
            System.out.println(e.getMessage());
        }

        // Find the maximum of two numbers
        System.out.println("Maximum of 5 and 10: " + max(5, 10));

        // Calculate the factorial of a number
        System.out.println("Factorial of 5: " + factorial(5));

        // Check if a number is prime
        System.out.println("Is 17 prime: " + isPrime(17));

        // Find the greatest common divisor of two numbers
        System.out.println("Greatest common divisor of 12 and 18: " + gcd(12, 18));

        // Find the least common multiple of two numbers
        System.out.println("Least common multiple of 12 and 18: " + lcm(12, 18));

        // Generate a random number between two numbers
        System.out.println("Random number between 1 and 10: " + random(1, 10));

        // Sort an array of integers
        int[] arr = {5, 2, 8, 3, 1, 4, 7, 6, 9};
        System.out.println("Unsorted array:");
        print(arr);
        sort(arr);
        System.out.println("Sorted array:");
        print(arr);

        // Reverse an array of integers
        reverse(arr);
        System.out.println("Reversed array:");
        print(arr);

        // Search for an element in an array of integers
        int target = 5;
        int index = search(arr, target);
        if (index != -1) {
            System.out.println("Found " + target + " at index " + index);
        } else {
            System.out.println("Could not find " + target);
        }
    }
}
```

This code demonstrates a variety of complex coding techniques in Java, including:

1. Custom exception handling
2. Generic methods
3. Recursive functions
4. Mathematical functions (isPrime, gcd, lcm, factorial)
5. Random number generation
6. Array sorting and searching
7. Input and output operations

The code also includes extensive comments to explain each section and function.