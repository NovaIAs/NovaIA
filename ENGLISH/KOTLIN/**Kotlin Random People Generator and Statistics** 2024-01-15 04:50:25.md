```kotlin
// Import required Kotlin libraries
import java.util.*
import java.math.BigInteger
import kotlin.collections.HashMap

// Define a custom data class to represent a Person
data class Person(val name: String, val age: Int)

// Define a function to generate a random integer between two values
fun randomNumber(min: Int, max: Int): Int {
    return Random().nextInt(max - min + 1) + min
}

// Define a function to generate a random string of a given length
fun randomString(length: Int): String {
    val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    return String(CharArray(length) { chars[randomNumber(0, chars.length - 1)] })
}

// Define a function to generate a list of random Person objects
fun generateRandomPeople(count: Int): List<Person> {
    return (1..count).map { Person(randomString(10), randomNumber(18, 65)) }
}

// Define a function to calculate the average age of a list of people
fun averageAge(people: List<Person>): Double {
    return people.sumBy { it.age } / people.size.toDouble()
}

// Define a function to group people by their age range
fun groupByAgeRange(people: List<Person>): Map<String, List<Person>> {
    val ageRanges = listOf("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
    return people.groupBy {
        when (it.age) {
            in 18..24 -> ageRanges[0]
            in 25..34 -> ageRanges[1]
            in 35..44 -> ageRanges[2]
            in 45..54 -> ageRanges[3]
            in 55..64 -> ageRanges[4]
            else -> ageRanges[5]
        }
    }
}

// Define a function to print the results
fun printResults(people: List<Person>) {
    println("Generated ${people.size} random people:")
    people.forEach { println(it) }
    println("Average age: ${averageAge(people)}")
    groupByAgeRange(people).forEach { (range, people) ->
        println("Age range $range:")
        people.forEach { println("\t$it") }
    }
}

// Generate a list of 100 random people
val people = generateRandomPeople(100)

// Print the results
printResults(people)
```

This code:

1. Defines a custom data class `Person` to represent a person with a name and age.

2. Defines functions to generate random integers, strings, and lists of random `Person` objects.

3. Defines functions to calculate the average age of a list of people and group them by their age range.

4. Generates a list of 100 random people and prints the results, including the average age and people grouped by age range.

This code demonstrates complex data structures, functions, and algorithms, and shows how to work with custom data classes, lists, and maps in Kotlin. It also demonstrates how to generate random data, perform calculations, and print the results in a structured format.