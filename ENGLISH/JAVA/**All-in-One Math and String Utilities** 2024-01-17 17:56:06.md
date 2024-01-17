```java
import java.util.*;
import java.lang.*;

// Define a custom exception class for division by zero
class DivideByZeroException extends Exception {
    public DivideByZeroException(String message) {
        super(message);
    }
}

// Define a custom exception class for array index out of bounds
class ArrayIndexOutOfBoundsException extends Exception {
    public ArrayIndexOutOfBoundsException(String message) {
        super(message);
    }
}

// Main class
public class ComplexCode {

    // Method to calculate the factorial of a number
    public static int factorial(int n) throws DivideByZeroException {
        if (n < 0) {
            throw new DivideByZeroException("Factorial is undefined for negative numbers");
        }
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    // Method to find the greatest common divisor of two numbers
    public static int gcd(int a, int b) {
        if (b == 0) {
            return a;
        } else {
            return gcd(b, a % b);
        }
    }

    // Method to find the least common multiple of two numbers
    public static int lcm(int a, int b) {
        return (a * b) / gcd(a, b);
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

    // Method to find the nth Fibonacci number
    public static int fibonacci(int n) {
        if (n < 0) {
            throw new ArrayIndexOutOfBoundsException("Fibonacci sequence is defined only for non-negative integers");
        }
        if (n <= 1) {
            return n;
        } else {
            return fibonacci(n - 1) + fibonacci(n - 2);
        }
    }

    // Method to find the sum of the digits of a number
    public static int sumOfDigits(int n) {
        if (n < 0) {
            throw new ArrayIndexOutOfBoundsException("Sum of digits is defined only for non-negative integers");
        }
        if (n == 0) {
            return 0;
        } else {
            return n % 10 + sumOfDigits(n / 10);
        }
    }

    // Method to reverse a string
    public static String reverseString(String str) {
        if (str == null) {
            throw new NullPointerException("Cannot reverse a null string");
        }
        if (str.isEmpty()) {
            return str;
        } else {
            return reverseString(str.substring(1)) + str.charAt(0);
        }
    }

    // Method to find the longest common subsequence of two strings
    public static String longestCommonSubsequence(String str1, String str2) {
        if (str1 == null || str2 == null) {
            throw new NullPointerException("Cannot find LCS of null strings");
        }
        int m = str1.length();
        int n = str2.length();
        int[][] dp = new int[m + 1][n + 1];
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1] + 1;
                } else {
                    dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
                }
            }
        }
        int i = m;
        int j = n;
        StringBuilder sb = new StringBuilder();
        while (i > 0 && j > 0) {
            if (str1.charAt(i - 1) == str2.charAt(j - 1)) {
                sb.append(str1.charAt(i - 1));
                i--;
                j--;
            } else {
                if (dp[i - 1][j] > dp[i][j - 1]) {
                    i--;
                } else {
                    j--;
                }
            }
        }
        return sb.reverse().toString();
    }

    // Main method
    public static void main(String[] args) {
        try {
            System.out.println("Factorial of 5: " + factorial(5));
            System.out.println("GCD of 12 and 18: " + gcd(12, 18));
            System.out.println("LCM of 12 and 18: " + lcm(12, 18));
            System.out.println("Is 17 prime: " + isPrime(17));
            System.out.println("10th Fibonacci number: " + fibonacci(10));
            System.out.println("Sum of digits of 12345: " + sumOfDigits(12345));
            System.out.println("Reverse of \"Hello\": " + reverseString("Hello"));
            System.out.println("LCS of \"ABCD\" and \"ACED\": " + longestCommonSubsequence("ABCD", "ACED"));
        } catch (DivideByZeroException e) {
            System.err.println(e.getMessage());
        } catch (ArrayIndexOutOfBoundsException e) {
            System.err.println(e.getMessage());
        } catch (NullPointerException e) {
            System.err.println(e.getMessage());
        }
    }
}
```

Explanation:

1. **Factorial:** The `factorial` method calculates the factorial of a given number. It uses recursion to compute the factorial, handling negative numbers and returning 1 for 0.

2. **Greatest Common Divisor (GCD):** The `gcd` method finds the greatest common divisor of two numbers using the Euclidean algorithm. It repeatedly divides the larger number by the smaller number until the remainder is 0, then returns the last non-zero remainder.

3. **Least Common Multiple (LCM):** The `lcm` method calculates the least common multiple of two numbers using the formula `LCM = (A * B) / GCD(A, B)`.

4. **Primality Test:** The `isPrime` method checks if a given number is prime. It iterates through all numbers from 2 to the square root of the given number, checking if any of them divide the number evenly. If it finds a divisor, it returns `false`, otherwise it returns `true`.

5. **Fibonacci Sequence:** The `fibonacci` method calculates the nth Fibonacci number using recursion. It handles negative inputs and returns 0 for n=0, 1 for n=1, and the sum of the previous two Fibonacci numbers for n>1.

6. **Sum of Digits:** The `sumOfDigits` method finds the sum of the digits of a given number. It uses recursion to add the last digit to the sum of the remaining digits.

7. **String Reversal:** The `reverseString` method reverses a given string using recursion. It handles null inputs and empty strings, and returns the reversed string.

8. **Longest Common Subsequence (LCS):** The `longestCommonSubsequence` method finds the longest common subsequence of two given strings. It uses dynamic programming to compute the length of the LCS and then reconstructs the LCS using backtracking.

9. **Main Method:** The `main` method demonstrates the usage of the various methods defined above. It handles exceptions thrown by the methods and prints the results to the console.