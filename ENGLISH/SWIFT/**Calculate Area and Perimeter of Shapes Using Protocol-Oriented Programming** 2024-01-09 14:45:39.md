```swift
// Import necessary libraries
import Foundation
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

// Define a function to calculate the total area of a list of shapes
func totalArea(shapes: [Shape]) -> Double {
    var total = 0.0
    for shape in shapes {
        total += shape.area()
    }
    return total
}

// Define a function to calculate the total perimeter of a list of shapes
func totalPerimeter(shapes: [Shape]) -> Double {
    var total = 0.0
    for shape in shapes {
        total += shape.perimeter()
    }
    return total
}

// Create a list of shapes
let shapes: [Shape] = [
    Rectangle(width: 5.0, height: 10.0),
    Circle(radius: 3.0),
    Rectangle(width: 7.0, height: 12.0),
    Circle(radius: 4.0)
]

// Calculate the total area and perimeter of the list of shapes
let totalAreaValue = totalArea(shapes: shapes)
let totalPerimeterValue = totalPerimeter(shapes: shapes)

// Print the results
print("Total area: \(totalAreaValue)")
print("Total perimeter: \(totalPerimeterValue)")
```

Explanation:

1. **Import Libraries**: The code begins by importing necessary libraries, including `Foundation` and `UIKit`.

2. **Define a Shape Protocol**: A protocol called `Shape` is defined, which declares two methods - `area()` and `perimeter()` - that must be implemented by any class conforming to this protocol.

3. **Define Rectangle Class**: The `Rectangle` class is defined, which conforms to the `Shape` protocol. It has properties `width` and `height`, and implements the `area()` and `perimeter()` methods to calculate the area and perimeter of a rectangle.

4. **Define Circle Class**: Similarly, the `Circle` class is defined, which also conforms to the `Shape` protocol. It has a property `radius` and implements the `area()` and `perimeter()` methods for calculating the area and perimeter of a circle.

5. **Define Helper Functions**: Two helper functions, `totalArea()` and `totalPerimeter()`, are defined to calculate the total area and perimeter of a list of shapes, respectively.

6. **Create a List of Shapes**: A list of shapes is created, containing two rectangles and two circles.

7. **Calculate Total Area and Perimeter**: The `totalArea()` and `totalPerimeter()` functions are called to calculate the total area and perimeter of the list of shapes.

8. **Print the Results**: The calculated values are printed to the console.

This code demonstrates object-oriented programming principles, protocol-oriented programming, and the use of generics in Swift. It provides a more complex and detailed example of code generation compared to the previous simple code.