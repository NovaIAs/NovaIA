```d
import std.stdio, std.algorithm, std.string, std.traits;

/*
 * Computes the factorial of a non-negative integer.
 */
long factorial(long n) {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

/*
 * Checks if a string is a palindrome.
 */
bool isPalindrome(string s) {
    return s == s.reverse;
}

/*
 * Finds the longest common substring between two strings.
 */
string longestCommonSubstring(string a, string b) {
    // Create a matrix to store the lengths of the longest common substrings
    // of the prefixes of a and b.
    int[][] lcsMatrix = new int[a.length][b.length];

    // Initialize the first row and column of the matrix to 0.
    for (int i = 0; i < a.length; i++) {
        lcsMatrix[i][0] = 0;
    }
    for (int j = 0; j < b.length; j++) {
        lcsMatrix[0][j] = 0;
    }

    // Fill in the rest of the matrix.
    for (int i = 1; i < a.length; i++) {
        for (int j = 1; j < b.length; j++) {
            if (a[i] == b[j]) {
                lcsMatrix[i][j] = lcsMatrix[i - 1][j - 1] + 1;
            } else {
                lcsMatrix[i][j] = 0;
            }
        }
    }

    // Find the maximum value in the matrix.
    int maxLcsLength = 0;
    int maxLcsRow = 0;
    int maxLcsCol = 0;
    for (int i = 0; i < a.length; i++) {
        for (int j = 0; j < b.length; j++) {
            if (lcsMatrix[i][j] > maxLcsLength) {
                maxLcsLength = lcsMatrix[i][j];
                maxLcsRow = i;
                maxLcsCol = j;
            }
        }
    }

    // Extract the longest common substring from the matrix.
    string lcs = "";
    while (maxLcsLength > 0) {
        lcs = a[maxLcsRow] ~ lcs;
        maxLcsRow--;
        maxLcsCol--;
        maxLcsLength--;
    }

    return lcs;
}

/*
 * Finds all the prime numbers between 1 and n.
 */
immutable(long)[] primes(long n) @safe pure {
    immutable(long)[] primes = new immutable(long)[n];
    long numPrimes = 0;
    for (long i = 2; i <= n; i++) {
        bool isPrime = true;
        for (immutable(long) prime in primes) {
            if (prime * prime > i) {
                break;
            }
            if (i % prime == 0) {
                isPrime = false;
                break;
            }
        }
        if (isPrime) {
            primes[numPrimes] = i;
            numPrimes++;
        }
    }
    return primes[0..numPrimes];
}

/*
 * Sorts an array of integers in ascending order.
 */
alias sortAscending = (immutable(long)[] array) @nogc pure {
    Mutable(immutable(long)[]) sorted = array.dup;
    std.algorithm.sort(sorted);
    return sorted;
};

/*
 * Sorts an array of strings in descending order.
 */
alias sortDescending = (immutable(string)[] array) @nogc pure {
    Mutable(immutable(string)[]) sorted = array.dup;
    std.algorithm.sort(sorted, std.traits.compareDescending);
    return sorted;
};

/*
 * Main function.
 */
void main() {
    // Print the factorial of 10.
    writeln("Factorial of 10:", factorial(10));

    // Check if "racecar" is a palindrome.
    writeln("Is \"racecar\" a palindrome?", isPalindrome("racecar"));

    // Find the longest common substring between "ABCD" and "ACBD".
    writeln("Longest common substring between \"ABCD\" and \"ACBD\":", longestCommonSubstring("ABCD", "ACBD"));

    // Print the prime numbers between 1 and 100.
    writeln("Prime numbers between 1 and 100:", primes(100));

    // Sort an array of integers in ascending order.
    immutable(long)[] unsortedIntegers = [10, 5, 3, 8, 2, 9, 4, 7, 1, 6];
    writeln("Unsorted integers:", unsortedIntegers);
    immutable(long)[] sortedIntegers = sortAscending(unsortedIntegers);
    writeln("Sorted integers:", sortedIntegers);

    // Sort an array of strings in descending order.
    immutable(string)[] unsortedStrings = ["apple", "banana", "cherry", "durian", "elderberry",
                                            "fig", "grape", "honeydew", "jackfruit", "kiwi"];
    writeln("Unsorted strings:", unsortedStrings);
    immutable(string)[] sortedStrings = sortDescending(unsortedStrings);
    writeln("Sorted strings:", sortedStrings);
}
```

**Explanation:**

This code is a collection of useful functions and algorithms written in the D programming language. Here's a breakdown of what each part of the code does:

1. **Factorial Function:**
   - `factorial()` is a recursive function that computes the factorial of a non-negative integer `n`.
   - It uses the mathematical definition of factorial: `factorial(n) = n * factorial(n-1)`, with a base case of `factorial(0) = 1`.


2. **Palindrome Check:**
   - `isPalindrome()` is a function that checks if a given string `s` is a palindrome, which means it reads the same forward and backward.
   - It compares the string to its reverse using the `reverse` property and returns `true` if they are equal.


3. **Longest Common Substring:**
   - `longestCommonSubstring()` finds the longest substring that is common to two given strings `a` and `b`.
   - It uses dynamic programming to build a matrix of the lengths of the longest common substrings of the prefixes of `a` and `b`.
   - The longest common substring is then extracted from the matrix.


4. **Prime Numbers:**
   - `primes()` returns an immutable array of all prime numbers between 1 and `n`.
   - It uses a Sieve of Eratosthenes algorithm to efficiently filter out non-prime numbers.


5. **Sorting Algorithms:**
   - `sortAscending` and `sortDescending` are aliases for anonymous functions that sort an array of integers and strings, respectively.
   - They use the standard library's `sort()` function with the appropriate comparison function for ascending or descending order.


6. **Main Function:**
   - The `main()` function is the entry point of the program.
   - It demonstrates the usage of the defined functions by printing the factorial of 10, checking if "racecar" is a palindrome, finding the longest common substring between "ABCD" and "ACBD", printing prime numbers between 1 and 100, and sorting arrays of integers and strings.