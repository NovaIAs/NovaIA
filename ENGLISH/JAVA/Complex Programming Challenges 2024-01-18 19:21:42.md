```java
import java.util.*;
import java.lang.*;
import java.math.*;

public class ComplexCode {

    private static final String[] ROMAN_NUMERALS = {"I", "IV", "V", "IX", "X", "XL", "L", "XC", "C", "CD", "D", "CM", "M"};
    private static final int[] ROMAN_VALUES = {1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000};

    public static void main(String[] args) {
        // Print the Fibonacci sequence up to 100
        int[] fibonacciSequence = {0, 1};
        while (fibonacciSequence[fibonacciSequence.length - 1] < 100) {
            int nextNumber = fibonacciSequence[fibonacciSequence.length - 1] + fibonacciSequence[fibonacciSequence.length - 2];
            fibonacciSequence = Arrays.copyOf(fibonacciSequence, fibonacciSequence.length + 1);
            fibonacciSequence[fibonacciSequence.length - 1] = nextNumber;
        }
        System.out.println("Fibonacci sequence up to 100: " + Arrays.toString(fibonacciSequence));

        // Convert a number to Roman numerals
        int number = 1984;
        String romanNumeral = "";
        for (int i = ROMAN_VALUES.length - 1; i >= 0; i--) {
            while (number >= ROMAN_VALUES[i]) {
                number -= ROMAN_VALUES[i];
                romanNumeral += ROMAN_NUMERALS[i];
            }
        }
        System.out.println("Roman numeral for 1984: " + romanNumeral);

        // Find the largest prime factor of a number
        int n = 600851475143L;
        long largestPrimeFactor = 0;
        for (long i = 2; i <= n / 2; i++) {
            while (n % i == 0) {
                largestPrimeFactor = i;
                n /= i;
            }
        }
        if (n > 1) {
            largestPrimeFactor = n;
        }
        System.out.println("Largest prime factor of 600851475143: " + largestPrimeFactor);

        // Find all the Pythagorean triples up to a given number
        int limit = 1000;
        for (int a = 1; a < limit; a++) {
            for (int b = a + 1; b < limit; b++) {
                int c = (int) Math.sqrt(a * a + b * b);
                if (c < limit && a * a + b * b == c * c) {
                    System.out.println("Pythagorean triple: " + a + ", " + b + ", " + c);
                }
            }
        }

        // Generate a random permutation of an array
        int[] array = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        Random random = new Random();
        for (int i = 0; i < array.length; i++) {
            int randomIndex = random.nextInt(array.length);
            int temp = array[i];
            array[i] = array[randomIndex];
            array[randomIndex] = temp;
        }
        System.out.println("Random permutation of [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]: " + Arrays.toString(array));

        // Find the longest common substring of two strings
        String s1 = "ABCDGHI";
        String s2 = "ACDGHR";
        int lcsLength = 0;
        int lcsStartIndex = -1;
        for (int i = 0; i < s1.length(); i++) {
            for (int j = 0; j < s2.length(); j++) {
                int k = 0;
                while (i + k < s1.length() && j + k < s2.length() && s1.charAt(i + k) == s2.charAt(j + k)) {
                    k++;
                }
                if (k > lcsLength) {
                    lcsLength = k;
                    lcsStartIndex = i;
                }
            }
        }
        String lcs = s1.substring(lcsStartIndex, lcsStartIndex + lcsLength);
        System.out.println("Longest common substring of \"ABCDGHI\" and \"ACDGHR\": \"" + lcs + "\"");

        // Find the maximum subarray sum of an array
        int[] subarray = {