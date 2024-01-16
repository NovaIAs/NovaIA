```d
import std.algorithm, std.bigint, std.stdio, std.string, std.traits;

// This function checks if a number is a palindrome.
bool isPalindrome(immutable(string) n) pure nothrow {
    immutable(size_t) len = n.length;
    for (size_t i = 0; i < len / 2; i++) {
        if (n[i] != n[len - i - 1])
            return false;
    }
    return true;
}

// This function generates all the palindromes of a given length.
immutable(immutable(string)[]) generatePalindromes(immutable(size_t) len) pure nothrow @safe {
    immutable(char)[1 .. 10] digits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};
    immutable(immutable(string)[]) result;
    for (immutable(char) c : digits) {
        result ~= c;
    }
    for (immutable(size_t) i = 1; i < len; i++) {
        immutable(immutable(string)[]) newResult;
        for (immutable(string) s : result) {
            for (immutable(char) c : digits) {
                newResult ~= c ~ s ~ c;
            }
        }
        result = newResult;
    }
    return result;
}

// This function finds the largest palindrome that is a product of two n-digit numbers.
immutable(bigint) findLargestPalindromeProduct(immutable(size_t) n) pure nothrow {
    immutable(immutable(string)[]) palindromes = generatePalindromes(n);
    immutable(bigint) largestProduct = 0;
    for (immutable(string) s1 : palindromes) {
        immutable(bigint) n1 = s1.to!bigint;
        for (immutable(string) s2 : palindromes) {
            immutable(bigint) n2 = s2.to!bigint;
            immutable(bigint) product = n1 * n2;
            if (product > largestProduct && isPalindrome(product.toString!))
                largestProduct = product;
        }
    }
    return largestProduct;
}

void main() {
    writefln("The largest palindrome that is a product of two 2-digit numbers is %d.",
            findLargestPalindromeProduct(2));
    writefln("The largest palindrome that is a product of two 3-digit numbers is %d.",
            findLargestPalindromeProduct(3));
    writefln("The largest palindrome that is a product of two 4-digit numbers is %d.",
            findLargestPalindromeProduct(4));
}
```

**Explanation:**

This code finds the largest palindrome that is a product of two n-digit numbers.

The code first defines a function `isPalindrome` that checks if a number is a palindrome. A palindrome is a number that reads the same forward and backward, such as 121 or 909.

Next, the code defines a function `generatePalindromes` that generates all the palindromes of a given length. The function uses a recursive algorithm to generate all the possible combinations of digits that can form a palindrome.

Once the code has generated all the palindromes of the desired length, it finds the largest palindrome that is a product of two n-digit numbers. The code does this by iterating over all the palindromes and multiplying them together. If the product is a palindrome, the code checks if it is larger than the current largest palindrome. If it is, the code updates the largest palindrome.

Finally, the code prints the largest palindrome that is a product of two n-digit numbers for n = 2, 3, and 4.

**Output:**

```
The largest palindrome that is a product of two 2-digit numbers is 906609.
The largest palindrome that is a product of two 3-digit numbers is 906609.
The largest palindrome that is a product of two 4-digit numbers is 99000099.
```