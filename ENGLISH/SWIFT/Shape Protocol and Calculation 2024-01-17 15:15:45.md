import Foundation

// Define a protocol for a shape.
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a struct for a circle.
struct Circle: Shape {
    var radius: Double

    // Calculate the area of the circle.
    func area() -> Double {
        return Double.pi * radius * radius
    }

    // Calculate the perimeter of the circle.
    func perimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a struct for a rectangle.
struct Rectangle: Shape {
    var width: Double
    var height: Double

    // Calculate the area of the rectangle.
    func area() -> Double {
        return width * height
    }

    // Calculate the perimeter of the rectangle.
    func perimeter() -> Double {
        return 2 * (width + height)
    }
}

// Define a struct for a triangle.
struct Triangle: Shape {
    var sideA: Double
    var sideB: Double
    var sideC: Double

    // Calculate the area of the triangle.
    func area() -> Double {
        let semiPerimeter = (sideA + sideB + sideC) / 2
        return sqrt(semiPerimeter * (semiPerimeter - sideA) * (semiPerimeter - sideB) * (semiPerimeter - sideC))
    }

    // Calculate the perimeter of the triangle.
    func perimeter() -> Double {
        return sideA + sideB + sideC
    }
}

// Define a function to print the area and perimeter of a shape.
func printShapeInfo(shape: Shape) {
    print("Area: \(shape.area())")
    print("Perimeter: \(shape.perimeter())")
}

// Create a circle, a rectangle, and a triangle.
let circle = Circle(radius: 5.0)
let rectangle = Rectangle(width: 10.0, height: 20.0)
let triangle = Triangle(sideA: 3.0, sideB: 4.0, sideC: 5.0)

// Print the area and perimeter of each shape.
printShapeInfo(shape: circle)
printShapeInfo(shape: rectangle)
printShapeInfo(shape: triangle)

// This code defines a protocol for a shape, and three structs that implement the protocol: Circle, Rectangle, and Triangle.
// The protocol defines two methods, area() and perimeter(), which calculate the area and perimeter of the shape, respectively.
// The structs implement the methods for each specific shape.
// The code also defines a function, printShapeInfo(), which takes a shape as an argument and prints the area and perimeter of the shape.
// Finally, the code creates a circle, a rectangle, and a triangle, and calls the printShapeInfo() function on each shape to print the area and perimeter of each shape.