```swift
import Foundation

// Define a protocol for a shape.
protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

// Define a struct for a circle.
struct Circle: Shape {
    let radius: Double

    var area: Double {
        return Double.pi * radius * radius
    }

    var perimeter: Double {
        return 2.0 * Double.pi * radius
    }
}

// Define a struct for a rectangle.
struct Rectangle: Shape {
    let width: Double
    let height: Double

    var area: Double {
        return width * height
    }

    var perimeter: Double {
        return 2.0 * (width + height)
    }
}

// Define a struct for a triangle.
struct Triangle: Shape {
    let sideA: Double
    let sideB: Double
    let sideC: Double

    var area: Double {
        let s = (sideA + sideB + sideC) / 2.0
        return sqrt(s * (s - sideA) * (s - sideB) * (s - sideC))
    }

    var perimeter: Double {
        return sideA + sideB + sideC
    }
}

// Define a function to calculate the total area of an array of shapes.
func totalArea(shapes: [Shape]) -> Double {
    var total = 0.0
    for shape in shapes {
        total += shape.area
    }
    return total
}

// Define a function to calculate the total perimeter of an array of shapes.
func totalPerimeter(shapes: [Shape]) -> Double {
    var total = 0.0
    for shape in shapes {
        total += shape.perimeter
    }
    return total
}

// Create an array of shapes.
let shapes: [Shape] = [
    Circle(radius: 5.0),
    Rectangle(width: 10.0, height: 20.0),
    Triangle(sideA: 3.0, sideB: 4.0, sideC: 5.0)
]

// Calculate the total area and perimeter of the shapes.
let totalArea = totalArea(shapes: shapes)
let totalPerimeter = totalPerimeter(shapes: shapes)

// Print the results.
print("Total area: \(totalArea)")
print("Total perimeter: \(totalPerimeter)")
```

This code defines a protocol for a shape, and then defines three different shapes: a circle, a rectangle, and a triangle. It also defines functions to calculate the total area and perimeter of an array of shapes.

The code then creates an array of shapes, and uses the functions to calculate the total area and perimeter of the shapes. Finally, it prints the results.

This code is complex because it uses a protocol, structs, and functions to perform a complex task. It is also differentiated because it defines three different shapes, and uses functions to calculate the total area and perimeter of an array of shapes.