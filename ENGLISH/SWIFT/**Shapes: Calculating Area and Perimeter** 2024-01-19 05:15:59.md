```swift
import Foundation

// Define a protocol for a Shape object
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a class for a Rectangle object that conforms to the Shape protocol
class Rectangle: Shape {
    var width: Double
    var height: Double

    init(width: Double, height: Double) {
        self.width = width
        self.height = height
    }

    func area() -> Double {
        return width * height
    }

    func perimeter() -> Double {
        return 2 * (width + height)
    }
}

// Define a class for a Circle object that conforms to the Shape protocol
class Circle: Shape {
    var radius: Double

    init(radius: Double) {
        self.radius = radius
    }

    func area() -> Double {
        return Double.pi * radius * radius
    }

    func perimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a class for a Triangle object that conforms to the Shape protocol
class Triangle: Shape {
    var sideA: Double
    var sideB: Double
    var sideC: Double

    init(sideA: Double, sideB: Double, sideC: Double) {
        self.sideA = sideA
        self.sideB = sideB
        self.sideC = sideC
    }

    func area() -> Double {
        let s = (sideA + sideB + sideC) / 2
        return sqrt(s * (s - sideA) * (s - sideB) * (s - sideC))
    }

    func perimeter() -> Double {
        return sideA + sideB + sideC
    }
}

// Define a function to calculate the total area of an array of shapes
func totalArea(shapes: [Shape]) -> Double {
    var totalArea: Double = 0
    for shape in shapes {
        totalArea += shape.area()
    }
    return totalArea
}

// Define a function to calculate the total perimeter of an array of shapes
func totalPerimeter(shapes: [Shape]) -> Double {
    var totalPerimeter: Double = 0
    for shape in shapes {
        totalPerimeter += shape.perimeter()
    }
    return totalPerimeter
}

// Create an array of shapes
let shapes: [Shape] = [
    Rectangle(width: 10, height: 5),
    Circle(radius: 5),
    Triangle(sideA: 3, sideB: 4, sideC: 5)
]

// Calculate the total area and perimeter of the array of shapes
let totalArea = totalArea(shapes: shapes)
let totalPerimeter = totalPerimeter(shapes: shapes)

// Print the results
print("Total area:", totalArea)
print("Total perimeter:", totalPerimeter)
```

Explanation:

* We first define a protocol named `Shape` that declares two functions: `area()` and `perimeter()`. These functions are used to calculate the area and perimeter of a shape, respectively.

* We then define three classes: `Rectangle`, `Circle`, and `Triangle`. These classes conform to the `Shape` protocol and provide implementations for the `area()` and `perimeter()` functions.

* We define a function called `totalArea()` that takes an array of shapes as an argument and returns the total area of the shapes in the array.

* We define a function called `totalPerimeter()` that takes an array of shapes as an argument and returns the total perimeter of the shapes in the array.

* We create an array of shapes containing a rectangle, a circle, and a triangle.

* We call the `totalArea()` and `totalPerimeter()` functions to calculate the total area and perimeter of the array of shapes.

* We print the results to the console.

This code demonstrates how to define a protocol, create classes that conform to the protocol, and use functions to operate on objects of those classes. It also shows how to calculate the total area and perimeter of an array of shapes.