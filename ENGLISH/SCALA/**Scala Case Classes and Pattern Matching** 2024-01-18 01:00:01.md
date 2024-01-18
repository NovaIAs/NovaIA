```scala
// Define a case class to represent a person with name, age, and occupation
case class Person(name: String, age: Int, occupation: String)

// Define a sealed trait to represent the different types of shapes
sealed trait Shape

// Define a case class to represent a circle with a radius
case class Circle(radius: Double) extends Shape

// Define a case class to represent a rectangle with a width and height
case class Rectangle(width: Double, height: Double) extends Shape

// Define a case class to represent a triangle with three sides
case class Triangle(side1: Double, side2: Double, side3: Double) extends Shape

// Define a function to calculate the area of a shape
def area(shape: Shape): Double = shape match {
  case Circle(radius) => math.Pi * radius * radius
  case Rectangle(width, height) => width * height
  case Triangle(side1, side2, side3) =>
    val s = (side1 + side2 + side3) / 2
    math.sqrt(s * (s - side1) * (s - side2) * (s - side3))
}

// Define a function to print the details of a person
def printPerson(person: Person): Unit = {
  println(s"Name: ${person.name}")
  println(s"Age: ${person.age}")
  println(s"Occupation: ${person.occupation}")
}

// Define a function to print the area of a shape
def printArea(shape: Shape): Unit = {
  println(s"Area: ${area(shape)}")
}

// Create a list of people
val people = List(
  Person("Alice", 25, "Software Engineer"),
  Person("Bob", 30, "Doctor"),
  Person("Carol", 35, "Teacher")
)

// Create a list of shapes
val shapes = List(
  Circle(5.0),
  Rectangle(10.0, 20.0),
  Triangle(3.0, 4.0, 5.0)
)

// Print the details of each person
people.foreach(printPerson)

// Print the area of each shape
shapes.foreach(printArea)
```

Explanation:

This code defines a case class `Person` to represent a person with name, age, and occupation. It also defines a sealed trait `Shape` to represent different types of shapes, along with case classes `Circle`, `Rectangle`, and `Triangle` to represent specific shapes.

The `area` function calculates the area of a shape based on its type. The `printPerson` and `printArea` functions print the details of a person and the area of a shape, respectively.

The code then creates lists of people and shapes, and uses `foreach` to iterate over each list and print the details of each person and the area of each shape.

This code demonstrates the use of case classes, sealed traits, pattern matching, and higher-order functions in Scala.