```scala
// Define a class called Rectangle with properties for length and width
class Rectangle(val length: Double, val width: Double) {

  // Calculate the area of the rectangle
  def area: Double = length * width

  // Calculate the perimeter of the rectangle
  def perimeter: Double = 2 * (length + width)

  // Check if the rectangle is a square
  def isSquare: Boolean = length == width

  // Define a method to print the rectangle's properties
  def printProperties(): Unit = {
    println(s"Length: $length")
    println(s"Width: $width")
    println(s"Area: $area")
    println(s"Perimeter: $perimeter")
    println(s"Is Square: $isSquare")
  }
}

// Define a class called Circle with a property for radius
class Circle(val radius: Double) {

  // Calculate the area of the circle
  def area: Double = math.Pi * radius * radius

  // Calculate the circumference of the circle
  def circumference: Double = 2 * math.Pi * radius

  // Check if the circle is a unit circle (radius of 1)
  def isUnitCircle: Boolean = radius == 1

  // Define a method to print the circle's properties
  def printProperties(): Unit = {
    println(s"Radius: $radius")
    println(s"Area: $area")
    println(s"Circumference: $circumference")
    println(s"Is Unit Circle: $isUnitCircle")
  }
}

// Define a class called Shape with abstract methods for calculating area and perimeter
abstract class Shape {
  def area: Double
  def perimeter: Double
}

// Create a function to print the properties of a shape
def printShapeProperties(shape: Shape): Unit = {
  println("Shape Properties:")
  println(s"Area: ${shape.area}")
  println(s"Perimeter: ${shape.perimeter}")
}

// Create a list of shapes
val shapes = List(
  new Rectangle(5, 10),
  new Circle(3),
  new Rectangle(2, 2)
)

// Print the properties of each shape
shapes.foreach(printShapeProperties)
```

This code defines two classes, `Rectangle` and `Circle`, which represent geometric shapes with properties and methods for calculating their area, perimeter, and other properties. It also defines an abstract class `Shape` with abstract methods for calculating area and perimeter. The code then creates a list of shapes and prints the properties of each shape.

Here's a breakdown of the code:

1. **Define the `Rectangle` Class**:
   - The `Rectangle` class has properties for `length` and `width`.
   - It has methods for calculating `area`, `perimeter`, `isSquare`, and `printProperties` to print the rectangle's properties.

2. **Define the `Circle` Class**:
   - The `Circle` class has a property for `radius`.
   - It has methods for calculating `area`, `circumference`, `isUnitCircle`, and `printProperties` to print the circle's properties.

3. **Define the Abstract `Shape` Class**:
   - The `Shape` class is an abstract class with abstract methods for calculating `area` and `perimeter`.

4. **Define a Function to Print Shape Properties**:
   - The `printShapeProperties` function takes a `Shape` as an argument and prints its area and perimeter.

5. **Create a List of Shapes**:
   - A list of shapes is created, including instances of `Rectangle` and `Circle`.

6. **Print the Properties of Each Shape**:
   - The `shapes` list is iterated over, and the `printShapeProperties` function is called for each shape to print its properties.

This code demonstrates the use of classes, inheritance, abstract classes, and functional programming in Scala to represent and manipulate geometric shapes.