```kotlin
// Import the necessary libraries.
import java.util.Scanner
import java.util.ArrayList
import java.util.Collections
import java.util.Comparator

// Define the main function.
fun main(args: Array<String>) {
    // Create a Scanner object to read input from the console.
    val scanner = Scanner(System.`in`)

    // Create an ArrayList to store the names of the students.
    val students = ArrayList<String>()

    // Prompt the user to enter the names of the students.
    println("Enter the names of the students, separated by commas:")

    // Read the input from the console and split it into an array of strings.
    val input = scanner.nextLine()
    val names = input.split(",")

    // Add the names of the students to the ArrayList.
    for (name in names) {
        students.add(name.trim())
    }

    // Sort the ArrayList of students in alphabetical order.
    Collections.sort(students, Comparator { o1, o2 -> o1.compareTo(o2) })

    // Print the names of the students in alphabetical order.
    println("The names of the students in alphabetical order are:")
    for (student in students) {
        println(student)
    }
}
```

**Explanation:**

* The `main` function is the entry point of the program.

* The `Scanner` object is used to read input from the console.

* The `ArrayList` is used to store the names of the students.

* The `split()` method is used to split the input string into an array of strings.

* The `add()` method is used to add the names of the students to the ArrayList.

* The `sort()` method is used to sort the ArrayList of students in alphabetical order.

* The `Comparator` interface is used to specify how the ArrayList of students should be sorted.

* The `compareTo()` method is used to compare two strings.

* The `println()` method is used to print the names of the students to the console.