```swift
import UIKit

// Define a protocol for a shape
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a class for a rectangle
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

// Define a class for a circle
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

// Define a class for a triangle
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
        let semiperimeter = (sideA + sideB + sideC) / 2
        return sqrt(semiperimeter * (semiperimeter - sideA) * (semiperimeter - sideB) * (semiperimeter - sideC))
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
var shapes: [Shape] = []
shapes.append(Rectangle(width: 10.0, height: 5.0))
shapes.append(Circle(radius: 5.0))
shapes.append(Triangle(sideA: 3.0, sideB: 4.0, sideC: 5.0))

// Calculate the total area and perimeter of the shapes
let totalArea = totalArea(shapes: shapes)
let totalPerimeter = totalPerimeter(shapes: shapes)

// Print the results
print("Total area: \(totalArea)")
print("Total perimeter: \(totalPerimeter)")
```

This code defines a protocol for a shape, and then defines three classes for different types of shapes: rectangle, circle, and triangle. It also defines functions to calculate the total area and perimeter of an array of shapes. The code then creates an array of shapes, and calculates the total area and perimeter of the shapes. Finally, it prints the results.

This code is complex and differentiated because it uses a protocol to define a common interface for different types of shapes, and then defines three different classes for different types of shapes. It also defines functions to calculate the total area and perimeter of an array of shapes, and then creates an array of shapes and calculates the total area and perimeter of the shapes. Finally, it prints the results. This code is also complex because it uses a variety of different data types, including Double, Array, and Shape.