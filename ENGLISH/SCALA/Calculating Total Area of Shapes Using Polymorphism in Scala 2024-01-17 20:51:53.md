```scala
// Define a trait for a Shape interface with an abstract method to calculate the area
trait Shape {
  def area: Double
}

// Define a class for a Circle that implements the Shape trait and calculates its area based on its radius
class Circle(radius: Double) extends Shape {
  override def area: Double = math.Pi * radius * radius
}

// Define a class for a Rectangle that implements the Shape trait and calculates its area based on its length and width
class Rectangle(length: Double, width: Double) extends Shape {
  override def area: Double = length * width
}

// Define a function to calculate the total area of a list of shapes
def totalArea(shapes: List[Shape]): Double = {
  shapes.foldLeft(0.0)((acc, shape) => acc + shape.area)
}

// Create a list of shapes
val shapes = List(new Circle(2.0), new Rectangle(3.0, 4.0), new Circle(1.0))

// Calculate the total area of the shapes
val totalAreaOfShapes = totalArea(shapes)

// Print the total area
println(s"Total area of the shapes: ${totalAreaOfShapes}")
```

Explanation:

1. We define a trait called `Shape` which serves as an interface for different shapes. It contains an abstract method called `area` that each shape must implement to calculate its area.

2. We define two classes called `Circle` and `Rectangle` that extend the `Shape` trait. The `Circle` class takes a radius as a parameter and calculates its area using the formula `πr²`. The `Rectangle` class takes length and width as parameters and calculates its area using the formula `length * width`.

3. We define a function called `totalArea` that takes a list of shapes as a parameter. It uses the `foldLeft` method to accumulate the areas of all the shapes in the list and returns the total area.

4. We create a list of shapes containing a circle with a radius of 2, a rectangle with a length of 3 and a width of 4, and another circle with a radius of 1.

5. We call the `totalArea` function with our list of shapes as the argument and store the result in the variable `totalAreaOfShapes`.

6. Finally, we print the total area of the shapes using the `println` function.

This code demonstrates the use of traits, abstract methods, polymorphism, and functional programming techniques in Scala to calculate the total area of a list of shapes.