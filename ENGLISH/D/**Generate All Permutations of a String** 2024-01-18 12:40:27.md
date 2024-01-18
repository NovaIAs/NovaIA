```d
import std.stdio, std.string;

void main() {
    // Define a function to calculate the factorial of a number
    int factorial(int n) {
        if (n == 0) {
            return 1;
        } else {
            return n * factorial(n - 1);
        }
    }

    // Define a function to generate all permutations of a string
    string[] permutations(string str) {
        string[] result = new string[factorial(str.length)];
        permutationHelper(str, "", result, 0);
        return result;
    }

    // Helper function for generating permutations
    void permutationHelper(string str, string prefix, string[] result, int index) {
        if (index == str.length) {
            result[index] = prefix;
        } else {
            for (int i = 0; i < str.length; i++) {
                if (prefix.indexOf(str[i]) == -1) {
                    permutationHelper(str, prefix + str[i], result, index + 1);
                }
            }
        }
    }

    // Get input from the user
    writefln("Enter a string: ");
    string input = readLine().strip();

    // Generate all permutations of the input string
    string[] permutations = permutations(input);

    // Print the permutations
    writefln("Permutations of %s:", input);
    for (int i = 0; i < permutations.length; i++) {
        writefln("\t%s", permutations[i]);
    }
}
```

This code defines two functions: `factorial` and `permutations`. The `factorial` function calculates the factorial of a number, which is the product of all positive integers less than or equal to that number. The `permutations` function generates all permutations of a string, which are all possible arrangements of the characters in the string.

The `permutationHelper` function is a helper function for the `permutations` function. It recursively generates all permutations of a string by adding one character at a time to the prefix.

The main function gets input from the user, generates all permutations of the input string, and prints the permutations.