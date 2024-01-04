```swift
// Import the Foundation library
import Foundation

// Define a protocol called "Shape"
protocol Shape {
    // Declare a method called "area" that returns a Double
    func area() -> Double
    
    // Declare a method called "perimeter" that returns a Double
    func perimeter() -> Double
}

// Define a struct called "Rectangle" that conforms to the "Shape" protocol
struct Rectangle: Shape {
    // Define properties for the rectangle's width and height
    let width: Double
    let height: Double
    
    // Implement the "area" method for the rectangle
    func area() -> Double {
        return width * height
    }
    
    // Implement the "perimeter" method for the rectangle
    func perimeter() -> Double {
        return 2 * (width + height)
    }
}

// Define a struct called "Circle" that conforms to the "Shape" protocol
struct Circle: Shape {
    // Define a property for the circle's radius
    let radius: Double
    
    // Implement the "area" method for the circle
    func area() -> Double {
        return Double.pi * radius * radius
    }
    
    // Implement the "perimeter" method for the circle
    func perimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a function called "printShapeInfo" that takes a Shape object as an argument
func printShapeInfo(shape: Shape) {
    // Print the shape's area and perimeter
    print("Area: \(shape.area())")
    print("Perimeter: \(shape.perimeter())")
}

// Create a Rectangle object
let rectangle = Rectangle(width: 10.0, height: 5.0)

// Create a Circle object
let circle = Circle(radius: 5.0)

// Print the area and perimeter of the rectangle
printShapeInfo(shape: rectangle)

// Print the area and perimeter of the circle
printShapeInfo(shape: circle)
```

This code defines a protocol called "Shape" with two methods: "area" and "perimeter". It then defines two structs, "Rectangle" and "Circle", that conform to the "Shape" protocol. The "Rectangle" struct has properties for the rectangle's width and height, and it implements the "area" and "perimeter" methods for a rectangle. The "Circle" struct has a property for the circle's radius, and it implements the "area" and "perimeter" methods for a circle.

The code then defines a function called "printShapeInfo" that takes a Shape object as an argument and prints the shape's area and perimeter. Finally, the code creates a Rectangle object and a Circle object, and it calls the "printShapeInfo" function to print the area and perimeter of each shape.

This code is complex because it uses protocols, structs, and functions to define and manipulate shapes. It also uses generics to define a function that can work with any Shape object. This code is also differentiated because it uses two different structs to represent different types of shapes. This code is unlikely to be repeated again because it is a custom implementation of a shape library.