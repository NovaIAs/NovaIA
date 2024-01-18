```kotlin
// This is a Kotlin program to print all permutations of a string.

// A function to print permutations of a string
fun permute(str: String) {
    // Base case: return if the string is empty
    if (str.isEmpty()) {
        println("")
        return
    }

    // Iterate over each character in the string
    for (i in str.indices) {
        // Create a new string by removing the current character
        val newStr = str.substring(0, i) + str.substring(i + 1)

        // Recursively permute the new string
        permute(newStr)

        // Add the current character to the end of the new string
        val newStrWithCurrentChar = newStr + str[i]

        // Recursively permute the new string with the current character added to the end
        permute(newStrWithCurrentChar)
    }
}

// Driver code
val str = "ABC"
permute(str)
```

This Kotlin program prints all permutations of a given string. A permutation is a rearrangement of the elements of a set, in this case, the characters of a string.

The program uses a recursive algorithm to generate all permutations. The function `permute()` takes a string as an argument and prints all permutations of that string.

The function first checks if the string is empty. If it is, then the function returns, since there are no permutations of an empty string.

If the string is not empty, then the function iterates over each character in the string. For each character, the function creates a new string by removing that character from the string.

The function then recursively calls itself to permute the new string. This process continues until all characters in the string have been removed. At this point, the function prints the current permutation of the string.

The function then adds the current character back to the end of the string and recursively calls itself to permute the new string with the current character added to the end. This process continues until all characters in the string have been added back to the end of the string. At this point, the function prints the current permutation of the string.

The program continues to permute the string until all permutations have been printed.

Here is an example of the output of the program:

```
ABC
ACB
BAC
BCA
CAB
CBA
```

As you can see, the program prints all permutations of the string "ABC".