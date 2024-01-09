```swift
// Define a protocol for a Shape with a method to calculate its area
protocol Shape {
    func calculateArea() -> Double
}

// Define a class for a Circle that implements the Shape protocol
class Circle: Shape {
    var radius: Double
    
    init(radius: Double) {
        self.radius = radius
    }
    
    func calculateArea() -> Double {
        return Double.pi * radius * radius
    }
}

// Define a class for a Rectangle that implements the Shape protocol
class Rectangle: Shape {
    var length: Double
    var width: Double
    
    init(length: Double, width: Double) {
        self.length = length
        self.width = width
    }
    
    func calculateArea() -> Double {
        return length * width
    }
}

// Define a class for a Triangle that implements the Shape protocol
class Triangle: Shape {
    var base: Double
    var height: Double
    
    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }
    
    func calculateArea() -> Double {
        return 0.5 * base * height
    }
}

// Define a function to calculate the total area of an array of shapes
func calculateTotalArea(shapes: [Shape]) -> Double {
    var totalArea: Double = 0.0
    
    for shape in shapes {
        totalArea += shape.calculateArea()
    }
    
    return totalArea
}

// Create an array of shapes
let shapes: [Shape] = [
    Circle(radius: 5.0),
    Rectangle(length: 10.0, width: 5.0),
    Triangle(base: 5.0, height: 10.0)
]

// Calculate the total area of the shapes
let totalArea = calculateTotalArea(shapes: shapes)

// Print the total area
print("The total area of the shapes is \(totalArea)")
```

This code defines a protocol called `Shape` with a method called `calculateArea()`. The protocol is then implemented by three classes: `Circle`, `Rectangle`, and `Triangle`. Each class has its own implementation of the `calculateArea()` method, which calculates the area of the shape.

The code also defines a function called `calculateTotalArea()` that takes an array of shapes as input and returns the total area of the shapes. The function iterates over the array of shapes and calls the `calculateArea()` method on each shape to calculate its area. The total area is then returned.

Finally, the code creates an array of shapes and then calls the `calculateTotalArea()` function to calculate the total area of the shapes. The total area is then printed to the console.