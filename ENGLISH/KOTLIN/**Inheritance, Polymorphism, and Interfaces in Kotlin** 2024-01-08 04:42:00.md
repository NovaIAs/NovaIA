```kotlin
// Kotlin program to demonstrate inheritance, polymorphism,
// abstract class, and interfaces

// Abstract class Shape
abstract class Shape {
    // abstract method draw()
    abstract fun draw()
}

// Class Rectangle inheriting Shape
class Rectangle(private val length: Int, private val width: Int) : Shape() {
    // override draw() method
    override fun draw() {
        println("Drawing a rectangle with length $length and width $width")
    }
}

// Class Circle inheriting Shape
class Circle(private val radius: Int) : Shape() {
    // override draw() method
    override fun draw() {
        println("Drawing a circle with radius $radius")
    }
}

// Interface Drawable
interface Drawable {
    fun draw()
}

// Class Square implementing Drawable
class Square(private val side: Int) : Drawable {
    // override draw() method
    override fun draw() {
        println("Drawing a square with side $side")
    }
}

// Class Triangle implementing Drawable
class Triangle(private val base: Int, private val height: Int) : Drawable {
    // override draw() method
    override fun draw() {
        println("Drawing a triangle with base $base and height $height")
    }
}

// Main function
fun main(args: Array<String>) {
    // Create instances of Rectangle, Circle, Square, and Triangle
    val rectangle = Rectangle(4, 5)
    val circle = Circle(6)
    val square = Square(7)
    val triangle = Triangle(8, 9)

    // Create a list of Drawable objects
    val drawings: List<Drawable> = listOf(rectangle, circle, square, triangle)

    // Draw all the shapes
    drawAll(drawings)
}

// Function to draw all the Drawable objects in a list
fun drawAll(drawings: List<Drawable>) {
    for (drawing in drawings) {
        drawing.draw()
    }
}
```

**Explanation:**

1. We define an abstract class `Shape` with an abstract method `draw()`. This class represents the common functionality of all shapes.


2. We define two concrete classes `Rectangle` and `Circle` that inherit from `Shape`. These classes implement the `draw()` method to draw a rectangle and a circle, respectively.


3. We define an interface `Drawable` with a `draw()` method. This interface represents the ability to draw an object.


4. We define two classes `Square` and `Triangle` that implement the `Drawable` interface. These classes implement the `draw()` method to draw a square and a triangle, respectively.


5. In the `main()` function, we create instances of `Rectangle`, `Circle`, `Square`, and `Triangle`.


6. We create a list of `Drawable` objects and add all the shapes to the list.


7. We call the `drawAll()` function to draw all the shapes in the list.


8. The `drawAll()` function iterates over the list of `Drawable` objects and calls the `draw()` method on each object. This results in the shapes being drawn to the console.