```kotlin
// Kotlin program to demonstrate the use of a for-each loop

// Creating an array of integers
val numbers = arrayOf(1, 2, 3, 4, 5)

// Using a for-each loop to iterate over the array
for (number in numbers) {
    // Printing each number in the array
    println(number)
}

// Kotlin program to demonstrate the use of a while loop

// Initializing a variable to keep track of the current number
var number = 1

// Using a while loop to iterate until the number is greater than 5
while (number <= 5) {
    // Printing the current number
    println(number)

    // Incrementing the number by 1
    number++
}

// Kotlin program to demonstrate the use of a do-while loop

// Initializing a variable to keep track of the current number
var number = 1

// Using a do-while loop to iterate until the number is greater than 5
do {
    // Printing the current number
    println(number)

    // Incrementing the number by 1
    number++
} while (number <= 5)

// Kotlin program to demonstrate the use of a for loop

// Creating an array of integers
val numbers = arrayOf(1, 2, 3, 4, 5)

// Using a for loop to iterate over the array
for (i in 0 until numbers.size) {
    // Printing the current number
    println(numbers[i])
}

// Kotlin program to demonstrate the use of a range

// Creating a range of numbers from 1 to 5
val numbers = 1..5

// Using a for-each loop to iterate over the range
for (number in numbers) {
    // Printing the current number
    println(number)
}

// Kotlin program to demonstrate the use of a when expression

// Creating a variable to store the current day of the week
val dayOfWeek = "Tuesday"

// Using a when expression to determine the day of the week
when (dayOfWeek) {
    "Monday" -> println("It's Monday!")
    "Tuesday" -> println("It's Tuesday!")
    "Wednesday" -> println("It's Wednesday!")
    "Thursday" -> println("It's Thursday!")
    "Friday" -> println("It's Friday!")
    "Saturday" -> println("It's Saturday!")
    "Sunday" -> println("It's Sunday!")
    else -> println("Invalid day of the week!")
}

// Kotlin program to demonstrate the use of a lambda expression

// Creating a list of integers
val numbers = listOf(1, 2, 3, 4, 5)

// Using a lambda expression to filter the list and return only the even numbers
val evenNumbers = numbers.filter { it % 2 == 0 }

// Printing the even numbers
println(evenNumbers)

// Kotlin program to demonstrate the use of a higher-order function

// Creating a list of integers
val numbers = listOf(1, 2, 3, 4, 5)

// Using the map() higher-order function to square each number in the list
val squaredNumbers = numbers.map { it * it }

// Printing the squared numbers
println(squaredNumbers)

// Kotlin program to demonstrate the use of a data class

// Creating a data class to represent a person
data class Person(val name: String, val age: Int)

// Creating an instance of the Person data class
val person = Person("John Doe", 30)

// Printing the person's name and age
println("Name: ${person.name}")
println("Age: ${person.age}")

// Kotlin program to demonstrate the use of an extension function

// Creating an extension function to add a "greet()" method to the String class
fun String.greet() {
    println("Hello, $this!")
}

// Calling the "greet()" method on a string
"World".greet()

// Kotlin program to demonstrate the use of a companion object

// Creating a companion object for the Person class
companion object {
    fun createPerson(name: String, age: Int): Person {
        return Person(name, age)
    }
}

// Creating an instance of the Person class using the companion object
val person = Person.createPerson("John Doe", 30)

// Printing the person's name and age
println("Name: ${person.name}")
println("Age: ${person.age}")

// Kotlin program to demonstrate the use of a sealed class

// Creating a sealed class to represent different types of animals
sealed class Animal {
    class Dog : Animal()
    class Cat : Animal()
    class Bird : Animal()
}

// Creating an instance of the Dog class
val dog = Animal.Dog()

// Using a when expression to determine the type of animal
when (dog) {
    is Animal.Dog -> println("It's a dog!")
    is Animal.Cat -> println("It's a cat!")
    is Animal.Bird -> println("It's a bird!")
}
```

This code demonstrates the use of various language features in Kotlin, including loops, ranges, when expressions, lambda expressions, higher-order functions, data classes, extension functions, companion objects, and sealed classes. These features allow you to write concise and expressive code in Kotlin.