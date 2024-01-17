```swift
import Foundation

// Define a protocol for a Shape with a method to calculate its area.
protocol Shape {
    func calculateArea() -> Double
}

// Define a class for a Circle that conforms to the Shape protocol.
class Circle: Shape {
    var radius: Double

    init(radius: Double) {
        self.radius = radius
    }

    // Calculate the area of the circle.
    func calculateArea() -> Double {
        return Double.pi * radius * radius
    }
}

// Define a class for a Rectangle that conforms to the Shape protocol.
class Rectangle: Shape {
    var length: Double
    var width: Double

    init(length: Double, width: Double) {
        self.length = length
        self.width = width
    }

    // Calculate the area of the rectangle.
    func calculateArea() -> Double {
        return length * width
    }
}

// Define a class for a Triangle that conforms to the Shape protocol.
class Triangle: Shape {
    var base: Double
    var height: Double

    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }

    // Calculate the area of the triangle.
    func calculateArea() -> Double {
        return 0.5 * base * height
    }
}

// Define a function to calculate the total area of an array of shapes.
func calculateTotalArea(shapes: [Shape]) -> Double {
    var totalArea: Double = 0.0

    for shape in shapes {
        totalArea += shape.calculateArea()
    }

    return totalArea
}

// Create an array of shapes.
let shapes: [Shape] = [
    Circle(radius: 5.0),
    Rectangle(length: 10.0, width: 5.0),
    Triangle(base: 8.0, height: 12.0)
]

// Calculate the total area of the shapes.
let totalArea = calculateTotalArea(shapes: shapes)

// Print the total area.
print("Total area: \(totalArea)")
```

This code defines a protocol for a Shape with a method to calculate its area. It then defines three classes for different shapes (Circle, Rectangle, and Triangle) that conform to the Shape protocol. The code also defines a function to calculate the total area of an array of shapes. Finally, it creates an array of shapes, calculates the total area, and prints the result.