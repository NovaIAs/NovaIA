```kotlin
// Define a higher-order function that takes a lambda as an argument and returns a function
fun higherOrderFunction(lambda: (Int) -> Int): (Int) -> Int {
    // The returned function takes an integer as an argument and applies the lambda to it
    return { lambda(it) }
}

// Create a lambda that adds 1 to a given integer
val addOneLambda: (Int) -> Int = { it + 1 }

// Use the higher-order function to create a function that adds 1 to an integer
val addOneFunction: (Int) -> Int = higherOrderFunction(addOneLambda)

// Call the addOneFunction with the value 5
val result = addOneFunction(5)

// Print the result, which should be 6
println(result)

// Define a class that represents a person
class Person(val name: String, val age: Int) {
    // Define a method that returns the person's name
    fun getName(): String {
        return name
    }

    // Define a method that returns the person's age
    fun getAge(): Int {
        return age
    }
}

// Create a list of people
val people = listOf(
    Person("John", 25),
    Person("Mary", 30),
    Person("Bob", 35)
)

// Use the filter function to get a list of people who are over 30 years old
val peopleOver30 = people.filter { it.age > 30 }

// Print the names of the people over 30 years old
for (person in peopleOver30) {
    println(person.getName())
}

// Define a sealed class that represents a geometric shape
sealed class Shape {
    // Define a class that represents a circle
    class Circle(val radius: Double) : Shape()

    // Define a class that represents a square
    class Square(val sideLength: Double) : Shape()

    // Define a class that represents a rectangle
    class Rectangle(val width: Double, val height: Double) : Shape()
}

// Create a function that takes a shape as an argument and returns its area
fun getArea(shape: Shape): Double {
    when (shape) {
        is Shape.Circle -> return Math.PI * shape.radius * shape.radius
        is Shape.Square -> return shape.sideLength * shape.sideLength
        is Shape.Rectangle -> return shape.width * shape.height
    }
}

// Create a list of shapes
val shapes = listOf(
    Shape.Circle(5.0),
    Shape.Square(10.0),
    Shape.Rectangle(5.0, 10.0)
)

// Use the map function to get a list of the areas of the shapes
val areas = shapes.map { getArea(it) }

// Print the areas of the shapes
for (area in areas) {
    println(area)
}
```

Explanation:

This code demonstrates various concepts in Kotlin, including higher-order functions, lambda expressions, classes, object-oriented programming, generics, sealed classes, and the when expression.

1. Higher-Order Function:

```kotlin
fun higherOrderFunction(lambda: (Int) -> Int): (Int) -> Int {
    return { lambda(it) }
}
```
This function takes a lambda as an argument and returns a function that applies the lambda to its input. This allows us to create functions dynamically based on the lambda passed as an argument.

2. Lambda Expression:

```kotlin
val addOneLambda: (Int) -> Int = { it + 1 }
```
A lambda expression is an anonymous function that can be passed as an argument to other functions. Here, we define a lambda that adds 1 to a given integer.

3. Class:

```kotlin
class Person(val name: String, val age: Int) {
    fun getName(): String {
        return name
    }

    fun getAge(): Int {
        return age
    }
}
```
This class represents a person with properties for name and age, and methods to access those properties.

4. Object-Oriented Programming:

```kotlin
val people = listOf(
    Person("John", 25),
    Person("Mary", 30),
    Person("Bob", 35)
)

val peopleOver30 = people.filter { it.age > 30 }

for (person in peopleOver30) {
    println(person.getName())
}
```
Here, we create a list of people, use the filter function to get a list of people over 30 years old, and then iterate over the list to print the names of those people.

5. Generics:

```kotlin
sealed class Shape {
    class Circle(val radius: Double) : Shape()
    class Square(val sideLength: Double) : Shape()
    class Rectangle(val width: Double, val height: Double) : Shape()
}
```
This sealed class defines different types of geometric shapes (circle, square, and rectangle) and provides a common base class for them.

6. Sealed Classes:

```kotlin
sealed class Shape {
    // ...
}
```
A sealed class is a class that can only be subclassed within the same file. This is useful for creating hierarchies of classes that share a common base class.

7. When Expression:

```kotlin
fun getArea(shape: Shape): Double {
    when (shape) {
        is Shape.Circle -> return Math.PI * shape.radius * shape.radius
        is Shape.Square -> return shape.sideLength * shape.sideLength
        is Shape.Rectangle -> return shape.width * shape.height
    }
}
```
The when expression is used to evaluate different cases based on the type of the object. Here, it is used to calculate the area of a shape based on its type (circle, square, or rectangle).

Overall, this code demonstrates various features and concepts in the Kotlin programming language, showcasing its flexibility and expressiveness for building complex and maintainable applications.