```swift
// Define a protocol for a shape with a `getArea()` method
protocol Shape {
    func getArea() -> Double
}

// Define a struct for a circle with a radius property
struct Circle: Shape {
    var radius: Double
    
    // Calculate and return the area of the circle
    func getArea() -> Double {
        return Double.pi * radius * radius
    }
}

// Define a struct for a rectangle with width and height properties
struct Rectangle: Shape {
    var width: Double
    var height: Double
    
    // Calculate and return the area of the rectangle
    func getArea() -> Double {
        return width * height
    }
}

// Define a struct for a triangle with base and height properties
struct Triangle: Shape {
    var base: Double
    var height: Double
    
    // Calculate and return the area of the triangle
    func getArea() -> Double {
        return 0.5 * base * height
    }
}

// Define a function to calculate the total area of an array of shapes
func totalArea(shapes: [Shape]) -> Double {
    // Initialize the total area to 0
    var totalArea: Double = 0
    
    // Loop through each shape in the array
    for shape in shapes {
        // Add the area of the shape to the total area
        totalArea += shape.getArea()
    }
    
    // Return the total area
    return totalArea
}

// Create an array of shapes
let shapes: [Shape] = [
    Circle(radius: 5.0),
    Rectangle(width: 10.0, height: 20.0),
    Triangle(base: 15.0, height: 25.0)
]

// Calculate the total area of the shapes
let totalArea = totalArea(shapes: shapes)

// Print the total area
print("Total area: \(totalArea)")
```

This code defines a protocol, `Shape`, which requires any conforming type to have a `getArea()` method that returns a `Double` value representing the area of the shape.

It then defines three structs, `Circle`, `Rectangle`, and `Triangle`, which conform to the `Shape` protocol and provide implementations for the `getArea()` method that calculate the area of the respective shapes based on their properties.

Next, it defines a function, `totalArea()`, which takes an array of `Shape` objects as input and calculates the total area of all the shapes in the array by iterating through the array and adding the area of each shape to a running total.

Finally, it creates an array of `Shape` objects, consisting of a circle, a rectangle, and a triangle, and then calls the `totalArea()` function to calculate and print the total area of the shapes in the array.

This code demonstrates the use of protocols and polymorphism in Swift to create a generic function that can be used to calculate the total area of an array of different types of shapes.