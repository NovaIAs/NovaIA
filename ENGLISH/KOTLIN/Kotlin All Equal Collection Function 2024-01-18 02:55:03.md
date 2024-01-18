```kotlin
// Kotlin code to demonstrate the working of join() in Collection
// Function returns true if all the elements are the same

fun <T> Collection<T>.allEqual(): Boolean {
    // Initialize a variable to store the first element
    var first: T? = null

    // Iterate over the collection and check if all elements are equal to the first
    for (element in this) {
        // If the first element is null, assign it the value of the current element
        if (first == null) {
            first = element
        }

        // If the current element is not equal to the first, return false
        else if (element != first) {
            return false
        }
    }

    // If all elements are equal, return true
    return true
}

// Driver code
fun main(args: Array<String>) {
    // Create a list of integers
    val numbers = listOf(1, 2, 3, 4, 5)

    // Check if all elements in the list are equal
    if (numbers.allEqual()) {
        println("All elements in the list are equal")
    } else {
        println("Not all elements in the list are equal")
    }

    // Create a list of strings
    val names = listOf("John", "Mary", "Bob", "Alice")

    // Check if all elements in the list are equal
    if (names.allEqual()) {
        println("All elements in the list are equal")
    } else {
        println("Not all elements in the list are equal")
    }
}
```

Output:

```
Not all elements in the list are equal
Not all elements in the list are equal
```

Explanation:

The code defines a function `allEqual()` that takes a collection of elements as input and returns true if all the elements in the collection are equal. The function uses a variable `first` to store the first element in the collection. Then, it iterates over the collection and compares each element to `first`. If any element is not equal to `first`, the function returns false. Otherwise, it returns true.

In the `main()` function, two lists are created: one with integers and one with strings. The `allEqual()` function is then called on each list to check if all the elements in the list are equal. The output shows that all the elements in the list of integers are not equal, while all the elements in the list of strings are not equal.