```java
// Importing necessary libraries
import java.util.*;
import java.lang.*;
import java.math.*;

public class ComplexCode {

    // Method to calculate the factorial of a number
    public static BigInteger factorial(int n) {
        if (n == 0) {
            return BigInteger.ONE;
        }
        return BigInteger.valueOf(n).multiply(factorial(n - 1));
    }

    // Method to check if a number is prime
    public static boolean isPrime(int n) {
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

    // Method to generate all prime numbers up to a given number
    public static List<Integer> generatePrimes(int n) {
        List<Integer> primes = new ArrayList<>();
        for (int i = 2; i <= n; i++) {
            if (isPrime(i)) {
                primes.add(i);
            }
        }
        return primes;
    }

    // Method to find the greatest common divisor (GCD) of two numbers using the Euclidean algorithm
    public static int gcd(int a, int b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }

    // Method to find the least common multiple (LCM) of two numbers
    public static int lcm(int a, int b) {
        return a * b / gcd(a, b);
    }

    // Method to find all factors of a number
    public static Set<Integer> findFactors(int n) {
        Set<Integer> factors = new HashSet<>();
        for (int i = 1; i <= Math.sqrt(n); i++) {
            if (n % i == 0) {
                factors.add(i);
                factors.add(n / i);
            }
        }
        return factors;
    }

    // Method to check if a number is perfect
    public static boolean isPerfect(int n) {
        Set<Integer> factors = findFactors(n);
        int sum = 0;
        for (int factor : factors) {
            sum += factor;
        }
        return sum - n == n;
    }

    // Method to find all perfect numbers up to a given number
    public static List<Integer> findPerfectNumbers(int n) {
        List<Integer> perfectNumbers = new ArrayList<>();
        for (int i = 2; i <= n; i++) {
            if (isPerfect(i)) {
                perfectNumbers.add(i);
            }
        }
        return perfectNumbers;
    }

    // Method to calculate the sum of digits of a number
    public static int sumOfDigits(int n) {
        int sum = 0;
        while (n > 0) {
            sum += n % 10;
            n /= 10;
        }
        return sum;
    }

    // Method to find all amicable numbers up to a given number
    public static List<Pair<Integer, Integer>> findAmicableNumbers(int n) {
        List<Pair<Integer, Integer>> amicableNumbers = new ArrayList<>();
        for (int i = 2; i <= n; i++) {
            int sumOfFactors = 0;
            for (int factor : findFactors(i)) {
                sumOfFactors += factor;
            }
            if (sumOfFactors != i) {
                int otherNumber = sumOfFactors;
                int sumOfFactors2 = 0;
                for (int factor : findFactors(otherNumber)) {
                    sumOfFactors2 += factor;
                }
                if (sumOfFactors2 == i) {
                    amicableNumbers.add(new Pair<>(i, otherNumber));
                }
            }
        }
        return amicableNumbers;
    }

    // Main method
    public static void main(String[] args) {
        // Calculate the factorial of a number
        int n = 5;
        System.out.println("Factorial of " + n + ": " + factorial(n));

        // Check if a number is prime
        n = 7;
        System.out.println("Is " + n + " prime? " + isPrime(n));

        // Generate all prime numbers up to a given number
        n = 100;
        List
