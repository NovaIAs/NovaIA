```swift
import Foundation

// Define a protocol for a shape with a method to calculate its area.
protocol Shape {
    func area() -> Double
}

// Define a class for a circle that implements the Shape protocol.
class Circle: Shape {
    var radius: Double

    init(radius: Double) {
        self.radius = radius
    }

    func area() -> Double {
        return Double.pi * radius * radius
    }
}

// Define a class for a square that implements the Shape protocol.
class Square: Shape {
    var sideLength: Double

    init(sideLength: Double) {
        self.sideLength = sideLength
    }

    func area() -> Double {
        return sideLength * sideLength
    }
}

// Define a class for a triangle that implements the Shape protocol.
class Triangle: Shape {
    var base: Double
    var height: Double

    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }

    func area() -> Double {
        return 0.5 * base * height
    }
}

// Define a function to calculate the total area of an array of shapes.
func totalArea(shapes: [Shape]) -> Double {
    var total = 0.0
    for shape in shapes {
        total += shape.area()
    }
    return total
}

// Create an array of shapes.
let shapes: [Shape] = [
    Circle(radius: 2.0),
    Square(sideLength: 3.0),
    Triangle(base: 4.0, height: 5.0)
]

// Calculate the total area of the shapes.
let total = totalArea(shapes: shapes)

// Print the total area.
print("Total area: \(total)")
```

Explanation:

* The code defines a `Shape` protocol with a method to calculate the area of a shape.
* The code defines three classes `Circle`, `Square`, and `Triangle` that implement the `Shape` protocol.
* The code defines a function `totalArea` to calculate the total area of an array of shapes.
* The code creates an array of shapes and calculates the total area of the shapes.
* The code prints the total area.