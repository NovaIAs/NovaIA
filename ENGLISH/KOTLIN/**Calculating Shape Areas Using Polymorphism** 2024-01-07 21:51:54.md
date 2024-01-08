```kotlin
sealed class Shape {
    object Circle : Shape()
    data class Rectangle(val width: Double, val height: Double) : Shape()
    data class Triangle(val sideA: Double, val sideB: Double, val sideC: Double) : Shape()
}

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

fun main() {
    val circle = Shape.Circle()
    val rectangle = Shape.Rectangle(5.0, 10.0)
    val triangle = Shape.Triangle(3.0, 4.0, 5.0)

    println("The area of the circle is ${calculateArea(circle)}")
    println("The area of the rectangle is ${calculateArea(rectangle)}")
    println("The area of the triangle is ${calculateArea(triangle)}")
}
```

Explanation:

1. We define a sealed class called `Shape` that represents different geometric shapes. This is a way to create a hierarchy of classes where each class represents a different type of shape.

2. Inside the `Shape` class, we define three subclasses:

   - `Circle`: Represents a circle.
   - `Rectangle`: Represents a rectangle.
   - `Triangle`: Represents a triangle.

3. Each subclass has its own properties that define the specific characteristics of that shape. For example, `Rectangle` has properties for width and height, while `Triangle` has properties for the lengths of its three sides.

4. We define a function called `calculateArea` that takes a `Shape` object as an argument and returns the area of that shape.

5. Inside the `calculateArea` function, we use the `when` expression to determine the type of the shape and then calculate the area based on the shape type.

6. For `Circle`, we use the formula for the area of a circle: `πr²`.
   For `Rectangle`, we use the formula for the area of a rectangle: `width * height`.
   For `Triangle`, we use the Heron's formula for the area of a triangle.

7. In the `main` function, we create instances of each shape and then call the `calculateArea` function to calculate the area of each shape.

8. Finally, we print the calculated areas to the console.

This code demonstrates how to use sealed classes and the `when` expression to create a polymorphic function that can handle different types of shapes.