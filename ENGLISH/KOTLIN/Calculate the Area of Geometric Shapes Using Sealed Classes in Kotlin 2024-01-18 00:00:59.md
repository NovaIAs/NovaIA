```kotlin
// Create a sealed class called Shape with three subclasses: Circle, Rectangle, and Triangle.
sealed class Shape {
    data class Circle(val radius: Double) : Shape()
    data class Rectangle(val width: Double, val height: Double) : Shape()
    data class Triangle(val sideA: Double, val sideB: Double, val sideC: Double) : Shape()
}

// Create a function called calculateArea that takes a Shape object and returns its area.
fun calculateArea(shape: Shape): Double {
    return when (shape) {
        is Shape.Circle -> Math.PI * shape.radius * shape.radius
        is Shape.Rectangle -> shape.width * shape.height
        is Shape.Triangle -> {
            val s = (shape.sideA + shape.sideB + shape.sideC) / 2
            Math.sqrt(s * (s - shape.sideA) * (s - shape.sideB) * (s - shape.sideC))
        }
    }
}

// Create a list of Shape objects.
val shapes = listOf(
    Shape.Circle(5.0),
    Shape.Rectangle(10.0, 20.0),
    Shape.Triangle(3.0, 4.0, 5.0)
)

// Print the area of each shape.
for (shape in shapes) {
    println("Area of $shape: ${calculateArea(shape)}")
}
```

Explanation:

1. We start by defining a sealed class called `Shape` with three subclasses: `Circle`, `Rectangle`, and `Triangle`. This allows us to represent different shapes in a common way.
2. Next, we create a function called `calculateArea` that takes a `Shape` object and returns its area. We use the `when` expression to handle each subclass of `Shape` and calculate the area accordingly.
3. We create a list of `Shape` objects and store it in the `shapes` variable. This list contains a circle, a rectangle, and a triangle.
4. Finally, we iterate over the `shapes` list and print the area of each shape using the `calculateArea` function.

Output:

```
Area of Circle(radius=5.0): 78.53981633974483
Area of Rectangle(width=10.0, height=20.0): 200.0
Area of Triangle(sideA=3.0, sideB=4.0, sideC=5.0): 6.0
```

This code demonstrates the use of sealed classes and the `when` expression to handle different types of objects in a concise and efficient manner. It also shows how to calculate the area of different shapes using polymorphism.