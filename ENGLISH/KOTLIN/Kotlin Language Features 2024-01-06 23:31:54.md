```kotlin
// Kotlin code to demonstrate the usage of various language features

// Class declaration with primary constructor and properties
class Person(val name: String, val age: Int) {

    // Companion object to hold factory methods
    companion object {

        // Factory method to create a Person instance
        fun create(name: String, age: Int): Person {
            return Person(name, age)
        }

        // Factory method to create a Person instance with a default age of 18
        fun createWithDefaultAge(name: String): Person {
            return Person(name, 18)
        }
    }

    // Function to greet the person
    fun greet() {
        println("Hello, my name is $name and I am $age years old.")
    }
}

// Class declaration with secondary constructor
class Student(name: String, age: Int, val major: String) : Person(name, age) {

    // Function to declare the student's major
    fun declareMajor() {
        println("My major is $major.")
    }
}

// Function to demonstrate the usage of lambda expressions
fun calculateSum(numbers: List<Int>): Int {
    return numbers.reduce { sum, number -> sum + number }
}

// Function to demonstrate the usage of higher-order functions
fun findMax(numbers: List<Int>, comparator: (Int, Int) -> Int): Int {
    return numbers.maxWithOrNull(comparator) ?: 0
}

// Function to demonstrate the usage of extension functions
fun String.capitalizeWords() = split(" ").joinToString(" ") { it.capitalize() }

// Function to demonstrate the usage of data classes
data class Point(val x: Int, val y: Int) {

    // Function to calculate the distance between two points
    fun distanceTo(other: Point): Double {
        return Math.sqrt(((other.x - x) * (other.x - x) + (other.y - y) * (other.y - y)).toDouble())
    }
}

// Function to demonstrate the usage of sealed classes
sealed class Shape {

    // Nested class representing a circle
    class Circle(val radius: Double) : Shape()

    // Nested class representing a rectangle
    class Rectangle(val width: Double, val height: Double) : Shape()

    // Nested class representing a triangle
    class Triangle(val sideA: Double, val sideB: Double, val sideC: Double) : Shape()
}

// Function to demonstrate the usage of when expressions
fun getShapeArea(shape: Shape): Double {
    return when (shape) {
        is Shape.Circle -> Math.PI * shape.radius * shape.radius
        is Shape.Rectangle -> shape.width * shape.height
        is Shape.Triangle -> {
            val s = (shape.sideA + shape.sideB + shape.sideC) / 2
            Math.sqrt(s * (s - shape.sideA) * (s - shape.sideB) * (s - shape.sideC))
        }
    }
}

// Main function to demonstrate the usage of various language features
fun main() {

    // Create a Person instance using the primary constructor
    val person1 = Person("John", 30)

    // Create a Person instance using the factory method
    val person2 = Person.create("Jane", 25)

    // Create a Person instance using the factory method with default age
    val person3 = Person.createWithDefaultAge("Bob")

    // Greet the persons
    person1.greet()
    person2.greet()
    person3.greet()

    // Create a Student instance
    val student1 = Student("Alice", 22, "Computer Science")

    // Greet the student and declare their major
    student1.greet()
    student1.declareMajor()

    // Calculate the sum of a list of numbers using a lambda expression
    val numbers = listOf(1, 2, 3, 4, 5)
    val sum = calculateSum(numbers)
    println("The sum of the numbers is $sum")

    // Find the maximum value in a list of numbers using a higher-order function
    val max = findMax(numbers) { a, b -> a.compareTo(b) }
    println("The maximum value is $max")

    // Capitalize the words in a string using an extension function
    val str = "hello world"
    val capitalizedStr = str.capitalizeWords()
    println("The capitalized string is $capitalizedStr")

    // Create a Point instance
    val point1 = Point(1, 2)

    // Create another Point instance
    val point2 = Point(4, 6)

    // Calculate the distance between the two points
    val distance = point1.distanceTo(point2)
    println("The distance between the two points is $distance")

    // Create a Shape instance (Circle)
    val circle = Shape.Circle(5.0)

    // Create another Shape instance (Rectangle)
    val rectangle = Shape.Rectangle(10.0, 20.0)

    // Create another Shape instance (Triangle)
    val triangle = Shape.Triangle(3.0, 4.0, 5.0)

    // Get the area of each shape using a when expression
    val circleArea = getShapeArea(circle)
    val rectangleArea = getShapeArea(rectangle)
    val triangleArea = getShapeArea(triangle)

    // Print the areas of the shapes
    println("The area of the circle is $circleArea")
    println("The area of the rectangle is $rectangleArea")
    println("The area of the triangle is $triangleArea")
}
```

This code demonstrates the usage of various language features in Kotlin, including:

* **Classes and Objects:** The code defines several classes, including `Person`, `Student`, `Point`, and `Shape`. It also shows the usage of primary and secondary constructors, factory methods, and extension functions.
* **Lambda Expressions:** The code demonstrates the usage of lambda expressions to define inline functions. For example, the `calculateSum()` function uses a lambda to calculate the sum of a list of numbers.
* **Higher-Order Functions:** The code demonstrates the usage of higher-order functions, which are functions that take other functions as arguments or return functions as results. For example, the `findMax()` function takes a comparator function as an argument to find the maximum value in a list.
* **Data Classes:** The code defines a data class called `Point` to represent a point in a 2D space. Data classes are a convenient way to define classes that hold data, and they provide useful features like automatic generation of getters and setters, `toString()`, and `equals()` methods.
* **Sealed Classes:** The code defines a sealed class called `Shape` to represent different shapes (Circle, Rectangle, and Triangle). Sealed classes are useful for representing restricted class hierarchies, where subclasses can only be defined within the same file as the sealed class.
* **When Expressions:** The code demonstrates the usage of when expressions to handle different cases based on the value of an expression. For example, the `getShapeArea()` function uses a when expression to calculate the area of a given shape based on its type.

This code provides a comprehensive overview of various language features in Kotlin and demonstrates how they can be used to solve real-world problems.