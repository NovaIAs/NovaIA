```kotlin
// Kotlin program to demonstrate the 
// Usage of break in labeled loops

fun main(args: Array<String>) {
    
    // Declaring an array of strings
    val arr = arrayOf("ONE", "TWO", "THREE")
    
    // Taking an input string from user
    val str = readLine()!!.toString()
    
    // Using labeled outer loop
    outer@ for (i in arr.indices) {
        
        // Using inner loop inside outer loop
        inner@ for (j in 0..str.length - 1) {
            
            // Checking if the element of array matches 
            // with the character of the string
            if (arr[i] == str[j].toString()) {
                
                // Printing the array element
                print(arr[i] + " ")
                
                // Breaking outer loop to stop 
                // as the character is found
                break@outer
            }
        }
    }
}
```

Explanation:

1. `val arr = arrayOf("ONE", "TWO", "THREE")`: This line creates an array of strings called `arr` and initializes it with three elements: `"ONE"`, `"TWO"`, and `"THREE"`.

2. `val str = readLine()!!.toString()`: This line reads a string from the user and stores it in the variable `str`. The `readLine()` function reads a line of text from the standard input (usually the keyboard) and returns it as a `String`. The `!!` operator is used to indicate that the `readLine()` function will never return null, and the `toString()` function is used to convert the `String?` to a non-nullable `String`.

3. `outer@ for (i in arr.indices)`: This line starts a labeled outer loop using the `outer` label. The `for` loop iterates over the indices of the `arr` array using the `indices` property. This means that the `i` variable will take on the values 0, 1, and 2, which are the indices of the three elements in the array.

4. `inner@ for (j in 0..str.length - 1)`: This line starts a nested inner loop using the `inner` label. The `for` loop iterates over the indices from 0 to `str.length - 1`. This means that the `j` variable will take on the values 0, 1, ..., `str.length - 1`, which are the indices of the characters in the `str` string.

5. `if (arr[i] == str[j].toString())`: Inside the inner loop, this line checks if the `i`th element of the `arr` array is equal to the `j`th character of the `str` string. The `toString()` method is used to convert the `Char` to a `String`.

6. `print(arr[i] + " ")`: If the condition in the `if` statement is true, this line prints the `i`th element of the `arr` array followed by a space.

7. `break@outer`: If the condition in the `if` statement is true, this line breaks out of the outer loop using the `break` statement. The `@outer` label is used to specify that the `break` statement should break out of the outer loop, not the inner loop.

8. The loop continues to iterate until the character is found or all the elements of the array are checked.

The output of this program will be a string containing the elements of the `arr` array that match the characters in the input string `str`, separated by spaces. For example, if the user enters the string `"OTE"`, the output will be `"ONE TWO"`.