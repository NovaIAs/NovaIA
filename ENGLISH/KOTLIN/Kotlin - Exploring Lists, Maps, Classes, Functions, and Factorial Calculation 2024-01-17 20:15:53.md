```kotlin
// Initialize a list of integers
val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Filter the list to get only even numbers
val evenNumbers = numbers.filter { it % 2 == 0 }

// Map the list of even numbers to their squares
val squaredEvenNumbers = evenNumbers.map { it * it }

// Print the squared even numbers
println("Squared even numbers: $squaredEvenNumbers")

// Initialize a map of names to their ages
val namesToAges = mapOf(
    "Alice" to 20,
    "Bob" to 25,
    "Carol" to 30,
    "Dave" to 35,
    "Eve" to 40
)

// Get the age of "Bob" from the map
val bobsAge = namesToAges["Bob"]

// Print Bob's age
println("Bob's age: $bobsAge")

// Initialize a class representing a person
class Person(val name: String, val age: Int)

// Create a list of people
val people = listOf(
    Person("Alice", 20),
    Person("Bob", 25),
    Person("Carol", 30),
    Person("Dave", 35),
    Person("Eve", 40)
)

// Filter the list of people to get only people who are older than 30
val oldPeople = people.filter { it.age > 30 }

// Map the list of old people to their names
val oldPeopleNames = oldPeople.map { it.name }

// Print the names of the old people
println("Names of old people: $oldPeopleNames")

// Initialize a function to calculate the factorial of a number
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n-1)
    }
}

// Calculate the factorial of 5
val factorialOf5 = factorial(5)

// Print the factorial of 5
println("Factorial of 5: $factorialOf5")
```

This code is a more complex and differentiated example of Kotlin code, which demonstrates a variety of features of the language, including:

* **Lists:** Lists are used to store collections of data. In this example, a list of numbers is created and then filtered to get only even numbers.
* **Maps:** Maps are used to store key-value pairs. In this example, a map of names to ages is created and then used to get the age of a specific person.
* **Classes:** Classes are used to define custom types. In this example, a class representing a person is created and then used to create a list of people.
* **Functions:** Functions are used to define reusable blocks of code. In this example, a function to calculate the factorial of a number is created and then used to calculate the factorial of 5.

This code demonstrates the power and flexibility of Kotlin as a programming language, and shows how it can be used to solve a variety of problems.