```scala
object ComplexScalaCode {

  // Define a sealed trait for different types of Shapes
  sealed trait Shape

  // Define case classes for different shapes: Circle, Rectangle, and Triangle
  case class Circle(radius: Double) extends Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Triangle(side1: Double, side2: Double, side3: Double) extends Shape

  // Define a function to calculate the area of a shape
  def calculateArea(shape: Shape): Double = shape match {
    case Circle(radius) => Math.PI * radius * radius
    case Rectangle(width, height) => width * height
    case Triangle(side1, side2, side3) => {
      val semiPerimeter = (side1 + side2 + side3) / 2
      Math.sqrt(semiPerimeter * (semiPerimeter - side1) * (semiPerimeter - side2) * (semiPerimeter - side3))
    }
  }

  // Define a function to print the details of a shape
  def printShapeDetails(shape: Shape): Unit = shape match {
    case Circle(radius) => println(s"Circle with radius $radius")
    case Rectangle(width, height) => println(s"Rectangle with width $width and height $height")
    case Triangle(side1, side2, side3) => println(s"Triangle with sides $side1, $side2, and $side3")
  }

  // Define a function to find the maximum area among a list of shapes
  def findMaxArea(shapes: List[Shape]): Double = {
    shapes.map(calculateArea).max
  }

  // Test the functions with various shapes
  val shapes = List(
    Circle(5.0),
    Rectangle(4.0, 10.0),
    Triangle(3.0, 4.0, 5.0),
    Circle(2.0),
    Rectangle(6.0, 8.0)
  )

  // Print the details of each shape
  println("Shapes:")
  shapes.foreach(printShapeDetails)

  // Find and print the maximum area
  val maxArea = findMaxArea(shapes)
  println(s"Maximum Area: $maxArea")
}
```

Explanation:

1. We define a sealed trait `Shape` to represent different types of shapes.

2. We define case classes for different shapes: `Circle`, `Rectangle`, and `Triangle`.

3. We define a function `calculateArea` to calculate the area of a shape using pattern matching.

4. We define a function `printShapeDetails` to print the details of a shape using pattern matching.

5. We define a function `findMaxArea` to find the maximum area among a list of shapes.

6. We create a list of shapes and store it in the variable `shapes`.

7. We use `foreach` to iterate through the list of shapes and print their details using `printShapeDetails`.

8. We find the maximum area among the shapes using `findMaxArea` and print the result.