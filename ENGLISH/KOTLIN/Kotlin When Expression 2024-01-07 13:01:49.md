```kotlin
// Kotlin program to demonstrate the usage of 'when' expression
// as a replacement for the 'if-else-if' condition

fun main(args: Array<String>) {
    // Creating a String variable
    val str = "Kotlin"

    // 'when' expression is used to check the value of str
    // and print appropriate message
    when (str) {
        "Java" -> println("It's Java")
        "Kotlin" -> println("It's Kotlin")
        else -> println("Neither Java nor Kotlin")
    }

    // Using 'when' expression with 'in' operator to check
    // if the value of str is present in a range
    when (str.length) {
        in 1..3 -> println("String length is between 1 and 3")
        in 4..6 -> println("String length is between 4 and 6")
        else -> println("String length is not in the specified range")
    }

    // Using 'when' expression to check the type of str
    when (str) {
        is String -> println("str is a String")
        is Int -> println("str is an Integer")
        else -> println("str is not a String or Integer")
    }

    // Using 'when' expression with a 'when' expression as the branch
    when (str) {
        "Java" -> println("It's Java")
        "Kotlin" -> when (str.length) {
            in 1..3 -> println("String length is between 1 and 3")
            in 4..6 -> println("String length is between 4 and 6")
            else -> println("String length is not in the specified range")
        }
        else -> println("Neither Java nor Kotlin")
    }
}
```

Explanation:

1. The first 'when' expression checks the value of the 'str' variable and prints "It's Java" if 'str' is equal to "Java", "It's Kotlin" if 'str' is equal to "Kotlin", and "Neither Java nor Kotlin" otherwise.

2. The second 'when' expression uses the 'in' operator to check if the length of 'str' is between 1 and 3, between 4 and 6, or not in the specified range.

3. The third 'when' expression checks the type of 'str' using the 'is' operator and prints "str is a String" if 'str' is a String, "str is an Integer" if 'str' is an Integer, and "str is not a String or Integer" otherwise.

4. The fourth 'when' expression combines the first and second 'when' expressions to check the value of 'str' and then check the length of 'str' if 'str' is equal to "Kotlin".