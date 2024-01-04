import Foundation

// Define a protocol for a Shape
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a struct for a Rectangle
struct Rectangle: Shape {
    var width: Double
    var height: Double

    func area() -> Double {
        return width * height
    }

    func perimeter() -> Double {
        return 2 * (width + height)
    }
}

// Define a struct for a Circle
struct Circle: Shape {
    var radius: Double

    func area() -> Double {
        return Double.pi * radius * radius
    }

    func perimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a function to calculate the total area of an array of shapes
func totalArea(shapes: [Shape]) -> Double {
    var total: Double = 0.0

    for shape in shapes {
        total += shape.area()
    }

    return total
}

// Define a function to calculate the total perimeter of an array of shapes
func totalPerimeter(shapes: [Shape]) -> Double {
    var total: Double = 0.0

    for shape in shapes {
        total += shape.perimeter()
    }

    return total
}

// Create an array of shapes
let shapes: [Shape] = [
    Rectangle(width: 5.0, height: 10.0),
    Circle(radius: 3.0),
    Rectangle(width: 2.0, height: 4.0),
    Circle(radius: 1.0)
]

// Calculate the total area and perimeter of the array of shapes
let totalArea = totalArea(shapes: shapes)
let totalPerimeter = totalPerimeter(shapes: shapes)

// Print the results
print("Total area: \(totalArea)")
print("Total perimeter: \(totalPerimeter)")

// Sample output:
// Total area: 94.24777960769379
// Total perimeter: 65.97347796076938