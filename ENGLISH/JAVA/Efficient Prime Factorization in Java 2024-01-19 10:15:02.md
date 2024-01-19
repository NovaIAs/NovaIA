```java

import java.util.*;
import java.io.*;
import java.lang.*;

class Efficient_Prime_Factorization {

    // Function to print prime factorization for a given number n
    public static void primeFactors(int n) {
        // Print the number itself if it is prime
        if (isPrime(n)) {
            System.out.println(n);
            return;
        }

        // Iterate over all odd integers from 3 to the square root of n
        for (int i = 3; i <= Math.sqrt(n); i += 2) {
            // If i is a prime factor of n, repeatedly divide n by i and print i
            while (n % i == 0) {
                System.out.println(i);
                n /= i;
            }
        }

        // If n is greater than 2, it must be a prime number greater than the square root of n
        if (n > 2) {
            System.out.println(n);
        }
    }

    // Function to check if a given number n is prime
    public static boolean isPrime(int n) {
        if (n <= 1) {
            return false;
        }
        if (n <= 3) {
            return true;
        }
        if (n % 2 == 0 || n % 3 == 0) {
            return false;
        }

        // Iterate over all odd integers from 5 to the square root of n
        for (int i = 5; i <= Math.sqrt(n); i += 6) {
            if (n % i == 0 || n % (i + 2) == 0) {
                return false;
            }
        }

        return true;
    }

    // Driver code to test the above functions
    public static void main(String[] args) {
        int n = 60;
        System.out.println("Prime factors of " + n + " are: ");
        primeFactors(n);
    }
}

```