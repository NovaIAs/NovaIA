```scala
object ComplexScalaCode {

  // Define a sealed trait to represent different types of shapes.
  sealed trait Shape

  // Define case classes for different types of shapes.
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Circle(radius: Double) extends Shape
  case class Triangle(side1: Double, side2: Double, side3: Double) extends Shape

  // Define a function to calculate the area of a shape.
  def calculateArea(shape: Shape): Double = {
    shape match {
      case Rectangle(width, height) => width * height
      case Circle(radius) => math.Pi * radius * radius
      case Triangle(side1, side2, side3) => {
        val semiperimeter = (side1 + side2 + side3) / 2.0
        math.sqrt(semiperimeter * (semiperimeter - side1) * (semiperimeter - side2) * (semiperimeter - side3))
      }
    }
  }

  // Define a function to find the largest shape in a list of shapes.
  def findLargestShape(shapes: List[Shape]): Shape = {
    shapes.maxBy(shape => calculateArea(shape))
  }

  // Define a function to group shapes by their type.
  def groupShapesByType(shapes: List[Shape]): Map[String, List[Shape]] = {
    shapes.groupBy(shape => shape.getClass.getSimpleName)
  }

  // Define a function to print the details of a shape.
  def printShapeDetails(shape: Shape): Unit = {
    shape match {
      case Rectangle(width, height) => println(s"Rectangle with width $width and height $height")
      case Circle(radius) => println(s"Circle with radius $radius")
      case Triangle(side1, side2, side3) => println(s"Triangle with sides $side1, $side2, and $side3")
    }
  }

  // Define a function to test the code.
  def testCode(): Unit = {
    // Create a list of shapes.
    val shapes = List(
      Rectangle(3.0, 4.0),
      Circle(2.0),
      Triangle(3.0, 4.0, 5.0)
    )

    // Print the details of each shape.
    shapes.foreach(shape => printShapeDetails(shape))

    // Find the largest shape in the list.
    val largestShape = findLargestShape(shapes)
    println(s"Largest shape: ")
    printShapeDetails(largestShape)

    // Group the shapes by their type.
    val groupedShapes = groupShapesByType(shapes)
    println(s"Shapes grouped by type:")
    groupedShapes.foreach(group => println(s"${group._1}: ${group._2}"))
  }

  // Call the testCode function to test the code.
  testCode()
}
```

Explanation of the code:

1. We define a sealed trait called `Shape` to represent different types of shapes.
2. We define case classes for different types of shapes: `Rectangle`, `Circle`, and `Triangle`.
3. We define a function called `calculateArea` to calculate the area of a shape.
4. We define a function called `findLargestShape` to find the largest shape in a list of shapes.
5. We define a function called `groupShapesByType` to group shapes by their type.
6. We define a function called `printShapeDetails` to print the details of a shape.
7. We define a function called `testCode` to test the code.
8. In the `testCode` function, we create a list of shapes, print the details of each shape, find the largest shape in the list, and group the shapes by their type.

This code demonstrates the use of sealed traits, case classes, pattern matching, higher-order functions, and other advanced features of the Scala programming language.