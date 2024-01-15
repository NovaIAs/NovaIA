```swift
// Import necessary frameworks
import Foundation
import UIKit

// Define a protocol for a shape
protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

// Define a class for a rectangle
class Rectangle: Shape {
    var width: Double
    var height: Double

    init(width: Double, height: Double) {
        self.width = width
        self.height = height
    }

    var area: Double {
        return width * height
    }

    var perimeter: Double {
        return 2 * (width + height)
    }
}

// Define a class for a circle
class Circle: Shape {
    var radius: Double

    init(radius: Double) {
        self.radius = radius
    }

    var area: Double {
        return Double.pi * radius * radius
    }

    var perimeter: Double {
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

    var area: Double {
        let semiperimeter = (sideA + sideB + sideC) / 2
        return sqrt(semiperimeter * (semiperimeter - sideA) * (semiperimeter - sideB) * (semiperimeter - sideC))
    }

    var perimeter: Double {
        return sideA + sideB + sideC
    }
}

// Create an array of shapes
var shapes: [Shape] = [
    Rectangle(width: 10.0, height: 5.0),
    Circle(radius: 5.0),
    Triangle(sideA: 3.0, sideB: 4.0, sideC: 5.0)
]

// Print the area and perimeter of each shape
for shape in shapes {
    print("Area: \(shape.area)")
    print("Perimeter: \(shape.perimeter)")
}

```

**Explanation:**

1. We define a protocol called `Shape` that has two properties: `area` and `perimeter`. Any class that conforms to this protocol must provide implementations for these properties.


2. We define three classes: `Rectangle`, `Circle`, and `Triangle` that conform to the `Shape` protocol. Each class has its own implementation of the `area` and `perimeter` properties based on the shape's specific formula.


3. We create an array of `Shape` objects called `shapes` and add instances of `Rectangle`, `Circle`, and `Triangle` to it.


4. We use a `for-in` loop to iterate over the `shapes` array and print the area and perimeter of each shape.

This code demonstrates how to define a protocol and create classes that conform to it. It also shows how to use a protocol to create an array of objects with different types but a common interface.