```kotlin
// Create a sealed class hierarchy to represent different types of shapes.
sealed class Shape

// Define subclasses for each specific shape.
class Circle(val radius: Double) : Shape()
class Rectangle(val width: Double, val height: Double) : Shape()
class Triangle(val side1: Double, val side2: Double, val side3: Double) : Shape()

// Create a function to calculate the area of a shape.
fun calculateArea(shape: Shape): Double {
    return when (shape) {
        is Circle -> Math.PI * shape.radius * shape.radius
        is Rectangle -> shape.width * shape.height
        is Triangle -> {
            val s = (shape.side1 + shape.side2 + shape.side3) / 2
            Math.sqrt(s * (s - shape.side1) * (s - shape.side2) * (s - shape.side3))
        }
    }
}

// Create a list of shapes.
val shapes = listOf(
    Circle(5.0),
    Rectangle(10.0, 20.0),
    Triangle(3.0, 4.0, 5.0)
)

// Calculate the total area of all shapes in the list.
val totalArea = shapes.sumByDouble { calculateArea(it) }

// Print the total area.
println("Total area: $totalArea")
```

This code demonstrates a more complex and differentiated Kotlin program that involves object-oriented programming, sealed classes, and functional programming techniques. Let's break down the code step by step:

1. **Define a Sealed Class Hierarchy for Shapes**:
   - We define a sealed class `Shape` to represent different types of shapes. This is a powerful feature in Kotlin that allows us to create a common interface for different classes representing various shapes.

2. **Create Subclasses for Specific Shapes**:
   - We create subclasses of `Shape` for specific shapes, such as `Circle`, `Rectangle`, and `Triangle`. These subclasses have properties representing the dimensions of each shape.

3. **Define a Function to Calculate Shape Area**:
   - We define a function `calculateArea` that takes a `Shape` object as a parameter and returns the area of that shape. The function uses `when` expression to handle different subclasses of `Shape` and calculate the area accordingly.

4. **Create a List of Shapes**:
   - We create a list of `Shape` objects called `shapes` that contains instances of `Circle`, `Rectangle`, and `Triangle`.

5. **Calculate Total Area Using Functional Programming**:
   - We use the `sumByDouble` function to calculate the total area of all shapes in the `shapes` list. This function is part of Kotlin's standard library and provides a concise way to perform aggregate operations on collections.

6. **Print the Total Area**:
   - Finally, we print the total area of all shapes.

This code demonstrates more advanced concepts in Kotlin, such as sealed classes, object-oriented programming, and functional programming with lambdas. It showcases how to create a complex program involving different types of objects and perform calculations on them.