```swift
// Define a protocol for a Shape
protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

// Define a class for a Circle that conforms to the Shape protocol
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

// Define a class for a Rectangle that conforms to the Shape protocol
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
        return 2 * width + 2 * height
    }
}

// Define a class for a Triangle that conforms to the Shape protocol
class Triangle: Shape {
    var base: Double
    var height: Double
    
    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }
    
    var area: Double {
        return 0.5 * base * height
    }
    
    var perimeter: Double {
        let side1 = sqrt(pow(base, 2) + pow(height, 2))
        let side2 = sqrt(pow(base, 2) + pow(height, 2))
        return base + side1 + side2
    }
}

// Define a function to calculate the total area of a list of shapes
func calculateTotalArea(shapes: [Shape]) -> Double {
    var totalArea: Double = 0.0
    for shape in shapes {
        totalArea += shape.area
    }
    return totalArea
}

// Define a function to calculate the total perimeter of a list of shapes
func calculateTotalPerimeter(shapes: [Shape]) -> Double {
    var totalPerimeter: Double = 0.0
    for shape in shapes {
        totalPerimeter += shape.perimeter
    }
    return totalPerimeter
}

// Create a list of shapes
let shapes: [Shape] = [
    Circle(radius: 5.0),
    Rectangle(width: 10.0, height: 20.0),
    Triangle(base: 15.0, height: 25.0)
]

// Calculate the total area of the shapes
let totalArea = calculateTotalArea(shapes: shapes)
print("Total area: \(totalArea)")

// Calculate the total perimeter of the shapes
let totalPerimeter = calculateTotalPerimeter(shapes: shapes)
print("Total perimeter: \(totalPerimeter)")
```

This code defines a protocol called `Shape` that specifies two properties: `area` and `perimeter`. It then defines three classes that conform to the `Shape` protocol: `Circle`, `Rectangle`, and `Triangle`. Each class has its own implementation of the `area` and `perimeter` properties.

The code also defines two functions: `calculateTotalArea` and `calculateTotalPerimeter`. These functions take a list of shapes as input and calculate the total area and perimeter of the shapes in the list, respectively.

Finally, the code creates a list of shapes and then calls the `calculateTotalArea` and `calculateTotalPerimeter` functions to calculate the total area and perimeter of the shapes in the list. The results are printed to the console.