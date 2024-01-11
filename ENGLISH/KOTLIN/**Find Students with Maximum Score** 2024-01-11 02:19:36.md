```kotlin
// Import necessary libraries
import java.util.Scanner
import java.util.ArrayList
import java.util.HashMap
import java.util.PriorityQueue

// Define the main function
fun main(args: Array<String>) {
    // Create a Scanner object to read input from the console
    val scanner = Scanner(System.`in`)

    // Create an ArrayList to store the names of the students
    val students = ArrayList<String>()

    // Create a HashMap to store the scores of the students
    val scores = HashMap<String, Int>()

    // Create a PriorityQueue to store the students' scores in descending order
    val pq = PriorityQueue<Int>(Collections.reverseOrder())

    // Read the number of students from the console
    val numStudents = scanner.nextInt()

    // Read the names and scores of the students from the console
    for (i in 0 until numStudents) {
        val name = scanner.next()
        val score = scanner.nextInt()
        students.add(name)
        scores[name] = score
        pq.add(score)
    }

    // Find the maximum score
    val maxScore = pq.peek()

    // Find the students who scored the maximum score
    val maxScorers = ArrayList<String>()
    for (student in students) {
        if (scores[student] == maxScore) {
            maxScorers.add(student)
        }
    }

    // Print the names of the students who scored the maximum score
    for (student in maxScorers) {
        println(student)
    }
}
```

**Explanation:**

This code reads the names and scores of a group of students from the console and finds the students who scored the maximum score. Here's a step-by-step explanation of the code:

1. Import necessary libraries:

```kotlin
import java.util.Scanner
import java.util.ArrayList
import java.util.HashMap
import java.util.PriorityQueue
```

These lines import the necessary libraries for input/output, data structures, and sorting.

2. Define the main function:

```kotlin
fun main(args: Array<String>) {
    // ...
}
```

This is the entry point of the program.

3. Create a Scanner object to read input from the console:

```kotlin
val scanner = Scanner(System.`in`)
```

This line creates a Scanner object that reads input from the standard input stream (the console).

4. Create an ArrayList to store the names of the students:

```kotlin
val students = ArrayList<String>()
```

This line creates an ArrayList to store the names of the students.

5. Create a HashMap to store the scores of the students:

```kotlin
val scores = HashMap<String, Int>()
```

This line creates a HashMap to store the scores of the students. The keys of the HashMap are the names of the students, and the values are their scores.

6. Create a PriorityQueue to store the students' scores in descending order:

```kotlin
val pq = PriorityQueue<Int>(Collections.reverseOrder())
```

This line creates a PriorityQueue to store the students' scores in descending order. The PriorityQueue is used to efficiently find the maximum score.

7. Read the number of students from the console:

```kotlin
val numStudents = scanner.nextInt()
```

This line reads the number of students from the console.

8. Read the names and scores of the students from the console:

```kotlin
for (i in 0 until numStudents) {
    val name = scanner.next()
    val score = scanner.nextInt()
    students.add(name)
    scores[name] = score
    pq.add(score)
}
```

This loop reads the names and scores of the students from the console. It adds the names to the ArrayList `students`, adds the scores to the HashMap `scores`, and adds the scores to the PriorityQueue `pq`.

9. Find the maximum score:

```kotlin
val maxScore = pq.peek()
```

This line finds the maximum score from the PriorityQueue `pq`.

10. Find the students who scored the maximum score:

```kotlin
val maxScorers = ArrayList<String>()
for (student in students) {
    if (scores[student] == maxScore) {
        maxScorers.add(student)
    }
}
```

This loop iterates through the names of the students and finds the students who scored the maximum score.

11. Print the names of the students who scored the maximum score:

```kotlin
for (student in maxScorers) {
    println(student)
}
```

This loop prints the names of the students who scored the maximum score.