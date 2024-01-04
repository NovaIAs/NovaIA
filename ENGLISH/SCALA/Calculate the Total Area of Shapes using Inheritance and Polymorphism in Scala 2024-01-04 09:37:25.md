```scala
// Define a class called "Shape"
class Shape {
  // Define a variable called "name" of type String
  var name: String = ""

  // Define a constructor that takes a parameter "name" of type String
  def this(name: String) {
    // Call the constructor of the parent class with the parameter "name"
    this()
    // Assign the value of the parameter "name" to the variable "name"
    this.name = name
  }

  // Define a method called "getArea" that returns a Double
  def getArea(): Double = {
    // Return 0.0 as the default area
    return 0.0
  }
}

// Define a class called "Rectangle" that extends the "Shape" class
class Rectangle(width: Double, height: Double) extends Shape("Rectangle") {
  // Define a variable called "width" of type Double
  var width: Double = 0.0

  // Define a variable called "height" of type Double
  var height: Double = 0.0

  // Initialize the "width" and "height" variables with the values passed as parameters to the constructor
  def this(width: Double, height: Double, name: String) {
    // Call the constructor of the parent class with the parameter "name"
    this(name)
    // Assign the values of the parameters "width" and "height" to the variables "width" and "height"
    this.width = width
    this.height = height
  }

  // Override the "getArea" method of the parent class
  override def getArea(): Double = {
    // Return the area of the rectangle (width * height)
    return width * height
  }
}

// Define a class called "Circle" that extends the "Shape" class
class Circle(radius: Double) extends Shape("Circle") {
  // Define a variable called "radius" of type Double
  var radius: Double = 0.0

  // Initialize the "radius" variable with the value passed as a parameter to the constructor
  def this(radius: Double, name: String) {
    // Call the constructor of the parent class with the parameter "name"
    this(name)
    // Assign the value of the parameter "radius" to the variable "radius"
    this.radius = radius
  }

  // Override the "getArea" method of the parent class
  override def getArea(): Double = {
    // Return the area of the circle (pi * radius^2)
    return Math.PI * radius * radius
  }
}

// Define a class called "ShapeCalculator"
class ShapeCalculator {
  // Define a method called "calculateTotalArea" that takes a list of shapes as a parameter and returns a Double
  def calculateTotalArea(shapes: List[Shape]): Double = {
    // Initialize a variable called "totalArea" to 0.0
    var totalArea: Double = 0.0

    // Iterate over the list of shapes
    for (shape <- shapes) {
      // Add the area of the current shape to the total area
      totalArea += shape.getArea()
    }

    // Return the total area
    return totalArea
  }
}

// Create a list of shapes
val shapes: List[Shape] = List(
  new Rectangle(5.0, 10.0, "Rectangle 1"),
  new Circle(5.0, "Circle 1"),
  new Rectangle(10.0, 15.0, "Rectangle 2"),
  new Circle(10.0, "Circle 2")
)

// Create an instance of the ShapeCalculator class
val calculator = new ShapeCalculator()

// Calculate the total area of the shapes
val totalArea = calculator.calculateTotalArea(shapes)

// Print the total area
println(s"Total Area: $totalArea")
```

This code defines three classes: `Shape`, `Rectangle`, and `Circle`. The `Shape` class is a parent class that defines common properties and methods for all shapes. The `Rectangle` and `Circle` classes are child classes that inherit from the `Shape` class and define specific properties and methods for rectangles and circles, respectively.

The `ShapeCalculator` class is used to calculate the total area of a list of shapes. It has a method called `calculateTotalArea` that takes a list of shapes as a parameter and returns the total area of all the shapes in the list.

The main method of the program creates a list of shapes, an instance of the `ShapeCalculator` class, and then calculates and prints the total area of the shapes.

This code demonstrates the use of inheritance, polymorphism, and method overriding in Scala. It also shows how to create and use a class to perform a specific task.