```swift
// Import necessary libraries
import Foundation
import UIKit

// Define a protocol for a Shape class
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a class for a Circle that conforms to the Shape protocol
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

// Define a class for a Rectangle that conforms to the Shape protocol
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

// Define a class for a Triangle that conforms to the Shape protocol
class Triangle: Shape {
    var side1: Double
    var side2: Double
    var side3: Double
    
    init(side1: Double, side2: Double, side3: Double) {
        self.side1 = side1
        self.side2 = side2
        self.side3 = side3
    }
    
    func area() -> Double {
        let semiperimeter = (side1 + side2 + side3) / 2
        let area = sqrt(semiperimeter * (semiperimeter - side1) * (semiperimeter - side2) * (semiperimeter - side3))
        return area
    }
    
    func perimeter() -> Double {
        return side1 + side2 + side3
    }
}

// Create an array of shapes
var shapes: [Shape] = []

// Add a Circle, Rectangle, and Triangle to the array
shapes.append(Circle(radius: 5.0))
shapes.append(Rectangle(width: 10.0, height: 20.0))
shapes.append(Triangle(side1: 3.0, side2: 4.0, side3: 5.0))

// Calculate the total area and perimeter of all shapes
var totalArea: Double = 0.0
var totalPerimeter: Double = 0.0

for shape in shapes {
    totalArea += shape.area()
    totalPerimeter += shape.perimeter()
}

// Print the total area and perimeter to the console
print("Total area: \(totalArea)")
print("Total perimeter: \(totalPerimeter)")
```

This code defines a protocol called `Shape` that specifies two methods: `area()` and `perimeter()`. These methods return the area and perimeter of a shape, respectively.

It then defines three classes: `Circle`, `Rectangle`, and `Triangle` that conform to the `Shape` protocol. Each class has its own implementation of the `area()` and `perimeter()` methods that calculate the area and perimeter of the specific shape.

The code creates an array called `shapes` and adds a Circle, Rectangle, and Triangle to the array. It then iterates over the array of shapes, calculating the total area and perimeter of all shapes.

Finally, the code prints the total area and perimeter to the console.

This code demonstrates the use of protocols and polymorphism in Swift to define a common interface for different types of shapes.