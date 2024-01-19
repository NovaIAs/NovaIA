// Import the UIKit framework
import UIKit

// Define a protocol for a shape
protocol Shape {
    // Calculate the area of the shape
    func calculateArea() -> Double
    
    // Calculate the perimeter of the shape
    func calculatePerimeter() -> Double
}

// Define a class for a circle
class Circle: Shape {
    // The radius of the circle
    let radius: Double
    
    // Initialize the circle with a radius
    init(radius: Double) {
        self.radius = radius
    }
    
    // Calculate the area of the circle
    func calculateArea() -> Double {
        return Double.pi * radius * radius
    }
    
    // Calculate the perimeter of the circle
    func calculatePerimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a class for a rectangle
class Rectangle: Shape {
    // The width of the rectangle
    let width: Double
    
    // The height of the rectangle
    let height: Double
    
    // Initialize the rectangle with a width and height
    init(width: Double, height: Double) {
        self.width = width
        self.height = height
    }
    
    // Calculate the area of the rectangle
    func calculateArea() -> Double {
        return width * height
    }
    
    // Calculate the perimeter of the rectangle
    func calculatePerimeter() -> Double {
        return 2 * (width + height)
    }
}

// Define a class for a triangle
class Triangle: Shape {
    // The base of the triangle
    let base: Double
    
    // The height of the triangle
    let height: Double
    
    // Initialize the triangle with a base and height
    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }
    
    // Calculate the area of the triangle
    func calculateArea() -> Double {
        return 0.5 * base * height
    }
    
    // Calculate the perimeter of the triangle
    func calculatePerimeter() -> Double {
        return base + height + sqrt(base * base + height * height)
    }
}

// Create an array of shapes
var shapes: [Shape] = []

// Add a circle to the array
shapes.append(Circle(radius: 5.0))

// Add a rectangle to the array
shapes.append(Rectangle(width: 10.0, height: 15.0))

// Add a triangle to the array
shapes.append(Triangle(base: 12.0, height: 8.0))

// Print the area and perimeter of each shape
for shape in shapes {
    print("Area: \(shape.calculateArea())")
    print("Perimeter: \(shape.calculatePerimeter())")
}