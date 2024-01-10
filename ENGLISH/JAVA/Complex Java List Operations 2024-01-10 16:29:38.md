```java
import java.util.*;
import java.lang.*;

public class ComplexJavaCode {

    // This method takes a list of integers and returns the sum of all the even numbers in the list.
    public static int sumEvenNumbers(List<Integer> numbers) {
        int sum = 0;
        for (int number : numbers) {
            if (number % 2 == 0) {
                sum += number;
            }
        }
        return sum;
    }

    // This method takes a list of strings and returns a map of each string to its length.
    public static Map<String, Integer> getStringLengths(List<String> strings) {
        Map<String, Integer> lengths = new HashMap<>();
        for (String string : strings) {
            lengths.put(string, string.length());
        }
        return lengths;
    }

    // This method takes a list of integers and returns the maximum value in the list.
    public static int findMax(List<Integer> numbers) {
        int max = Integer.MIN_VALUE;
        for (int number : numbers) {
            if (number > max) {
                max = number;
            }
        }
        return max;
    }

    // This method takes a list of strings and returns a list of the strings that are longer than 5 characters.
    public static List<String> filterLongStrings(List<String> strings) {
        List<String> longStrings = new ArrayList<>();
        for (String string : strings) {
            if (string.length() > 5) {
                longStrings.add(string);
            }
        }
        return longStrings;
    }

    // This method takes a list of integers and returns a list of the integers that are divisible by 3.
    public static List<Integer> filterDivisibleBy3(List<Integer> numbers) {
        List<Integer> divisibleBy3 = new ArrayList<>();
        for (int number : numbers) {
            if (number % 3 == 0) {
                divisibleBy3.add(number);
            }
        }
        return divisibleBy3;
    }

    // This method takes a list of strings and returns a list of the strings that start with the letter 'A'.
    public static List<String> filterStartsWithA(List<String> strings) {
        List<String> startsWithA = new ArrayList<>();
        for (String string : strings) {
            if (string.startsWith("A")) {
                startsWithA.add(string);
            }
        }
        return startsWithA;
    }

    // This method takes a list of integers and returns a list of the integers that are greater than 10 and less than 20.
    public static List<Integer> filterBetween10And20(List<Integer> numbers) {
        List<Integer> between10And20 = new ArrayList<>();
        for (int number : numbers) {
            if (number > 10 && number < 20) {
                between10And20.add(number);
            }
        }
        return between10And20;
    }

    // This method takes a list of strings and returns a list of the strings that contain the letter 'e'.
    public static List<String> filterContainsE(List<String> strings) {
        List<String> containsE = new ArrayList<>();
        for (String string : strings) {
            if (string.contains("e")) {
                containsE.add(string);
            }
        }
        return containsE;
    }

    // This method takes a list of integers and returns a list of the integers that are prime numbers.
    public static List<Integer> filterPrimeNumbers(List<Integer> numbers) {
        List<Integer> primeNumbers = new ArrayList<>();
        for (int number : numbers) {
            if (isPrime(number)) {
                primeNumbers.add(number);
            }
        }
        return primeNumbers;
    }

    // This method checks if a given number is a prime number.
    private static boolean isPrime(int number) {
        if (number <= 1) {
            return false;
        }
        for (int i = 2; i <= Math.sqrt(number); i++) {
            if (number % i == 0) {
                return false;
            }
        }
        return true;
    }

    public static void main(String[] args) {
        // Create a list of integers.
        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

        // Print the sum of all the even numbers in the list.
        int sumOfEvenNumbers = sumEvenNumbers(numbers);
        System.out.println("Sum of even numbers: " + sumOfEvenNumbers);

        // Create a list of strings.
        List<String> strings = Arrays.asList("Hello", "World", "Java", "Programming", "Language");

        // Print a map of each string to its length.
        Map<String, Integer> stringLengths = getStringLengths(strings);
        System.out.println("String lengths: " + stringLengths);

        // Print the maximum value in the list of integers.
        int maxNumber = findMax(numbers);
        System.out.println("Maximum value: " + maxNumber);

        // Print a list of the strings that are longer than 5 characters.
        List<String> longStrings = filterLongStrings(strings);
        System.out.println("Long strings: " + longStrings);

        // Print a list of the integers that are divisible by 3.
        List<Integer> divisibleBy3 = filterDivisibleBy3(numbers);
        System.out.println("Divisible by 3: " + divisibleBy3);

        // Print a list of the strings that start with the letter 'A'.
        List<String> startsWithA = filterStartsWithA(strings);
        System.out.println("Starts with A: " + startsWithA);

        // Print a list of the integers that are greater than 10 and less than 20.
        List<Integer> between10And20 = filterBetween10And20(numbers);
        System.out.println("Between 10 and 20: " + between10And20);

        // Print a list of the strings that contain the letter 'e'.
        List<String> containsE = filterContainsE(strings);
        System.out.println("Contains E: " + containsE);

        // Print a list of the integers that are prime numbers.
        List<Integer> primeNumbers = filterPrimeNumbers(numbers);
        System.out.println("Prime numbers: " + primeNumbers);
    }
}
```

This code performs a variety of operations on lists of integers and strings. It calculates the sum of even numbers, finds the maximum value, filters strings by length, divisibility, starts with, between range, contains character, and prime numbers. The code is well-commented and easy to understand.