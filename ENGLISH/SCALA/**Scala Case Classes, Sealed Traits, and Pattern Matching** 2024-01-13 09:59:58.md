```scala
// Define a case class to represent a person with name and age
case class Person(name: String, age: Int)

// Define a sealed trait to represent the different types of shapes
sealed trait Shape
// Define a case class to represent a circle with radius
case class Circle(radius: Double) extends Shape
// Define a case class to represent a rectangle with width and height
case class Rectangle(width: Double, height: Double) extends Shape

// Define a function to calculate the area of a shape
def area(shape: Shape): Double = {
  shape match {
    case Circle(radius) => math.Pi * radius * radius
    case Rectangle(width, height) => width * height
  }
}

// Define a function to print the details of a person
def printPerson(person: Person): Unit = {
  println(s"Name: ${person.name}")
  println(s"Age: ${person.age}")
}

// Define a function to print the area of a shape
def printArea(shape: Shape): Unit = {
  println(s"Area: ${area(shape)}")
}

// Create a list of persons
val persons = List(
  Person("Alice", 25),
  Person("Bob", 30),
  Person("Carol", 35)
)

// Create a list of shapes
val shapes = List(
  Circle(5.0),
  Rectangle(10.0, 20.0)
)

// Iterate over the list of persons and print their details
persons.foreach(printPerson)

// Iterate over the list of shapes and print their areas
shapes.foreach(printArea)
```

Explanation:

* We define a `Person` case class to represent a person with their name and age.
* We define a sealed trait `Shape` to represent different types of shapes and two case classes `Circle` and `Rectangle` to represent specific shapes.
* We define a function `area` that takes a shape and returns its area based on its type.
* We define functions `printPerson` and `printArea` to print the details of a person and the area of a shape, respectively.
* We create lists of persons and shapes and use `foreach` to iterate over them and print their details or areas.

This code demonstrates the use of case classes, sealed traits, pattern matching, and higher-order functions in Scala. It also showcases how to define and work with custom data types and perform operations on them.