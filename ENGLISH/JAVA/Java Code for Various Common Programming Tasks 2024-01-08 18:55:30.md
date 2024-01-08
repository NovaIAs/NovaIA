```java
import java.util.*;
import java.math.*;
import java.io.*;

public class ComplexCode {

    public static void main(String[] args) {
        // Read input from console
        Scanner scanner = new Scanner(System.in);
        System.out.println("Enter a number:");
        int n = scanner.nextInt();

        // Calculate factorial of n
        BigInteger factorial = calculateFactorial(n);

        // Print the result
        System.out.println("Factorial of " + n + " is " + factorial);

        // Find all prime numbers up to n
        List<Integer> primes = findPrimeNumbers(n);

        // Print the prime numbers
        System.out.println("Prime numbers up to " + n + ": " + primes);

        // Generate a random string of length n
        String randomString = generateRandomString(n);

        // Print the random string
        System.out.println("Random string of length " + n + ": " + randomString);

        // Sort an array of integers in ascending order
        int[] numbers = {1, 5, 3, 2, 4};
        Arrays.sort(numbers);

        // Print the sorted array
        System.out.println("Sorted array: " + Arrays.toString(numbers));

        // Find the maximum value in an array of integers
        int max = findMax(numbers);

        // Print the maximum value
        System.out.println("Maximum value in the array: " + max);

        // Find the minimum value in an array of integers
        int min = findMin(numbers);

        // Print the minimum value
        System.out.println("Minimum value in the array: " + min);

        // Reverse a string
        String string = "Hello world";
        String reversedString = reverseString(string);

        // Print the reversed string
        System.out.println("Reversed string: " + reversedString);

        // Convert a string to uppercase
        String upperCaseString = string.toUpperCase();

        // Print the upper case string
        System.out.println("Upper case string: " + upperCaseString);

        // Convert a string to lowercase
        String lowerCaseString = string.toLowerCase();

        // Print the lower case string
        System.out.println("Lower case string: " + lowerCaseString);

        // Check if a string is a palindrome
        boolean isPalindrome = isPalindrome(string);

        // Print the result
        System.out.println("Is \"" + string + "\" a palindrome? " + isPalindrome);

        // Check if a string is a number
        boolean isNumber = isNumber(string);

        // Print the result
        System.out.println("Is \"" + string + "\" a number? " + isNumber);

        // Check if a string is an email address
        