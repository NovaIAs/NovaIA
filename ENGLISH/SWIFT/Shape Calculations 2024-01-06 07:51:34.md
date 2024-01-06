```swift
protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

struct Square: Shape {
    var sideLength: Double

    var area: Double {
        return sideLength * sideLength
    }

    var perimeter: Double {
        return 4 * sideLength
    }
}

struct Circle: Shape {
    var radius: Double

    var area: Double {
        return Double.pi * radius * radius
    }

    var perimeter: Double {
        return 2 * Double.pi * radius
    }
}

struct Rectangle: Shape {
    var width: Double
    var height: Double

    var area: Double {
        return width * height
    }

    var perimeter: Double {
        return 2 * (width + height)
    }
}

struct Triangle: Shape {
    var sideLengthA: Double
    var sideLengthB: Double
    var sideLengthC: Double

    var area: Double {
        let semiperimeter = (sideLengthA + sideLengthB + sideLengthC) / 2
        return sqrt(semiperimeter * (semiperimeter - sideLengthA) * (semiperimeter - sideLengthB) * (semiperimeter - sideLengthC))
    }

    var perimeter: Double {
        return sideLengthA + sideLengthB + sideLengthC
    }
}

// Create an array of shapes
let shapes: [Shape] = [
    Square(sideLength: 5),
    Circle(radius: 3),
    Rectangle(width: 4, height: 6),
    Triangle(sideLengthA: 3, sideLengthB: 4, sideLengthC: 5)
]

// Calculate the total area and perimeter of all shapes
var totalArea = 0.0
var totalPerimeter = 0.0

for shape in shapes {
    totalArea += shape.area
    totalPerimeter += shape.perimeter
}

// Print the results
print("Total area: \(totalArea)")
print("Total perimeter: \(totalPerimeter)")
```

This code defines a protocol called `Shape` with two properties: `area` and `perimeter`. It then defines four different structs that conform to the `Shape` protocol: `Square`, `Circle`, `Rectangle`, and `Triangle`. Each struct has its own implementation of the `area` and `perimeter` properties.

Next, the code creates an array of shapes and calculates the total area and perimeter of all shapes in the array. Finally, the code prints the results.

This code is complex and differentiated because it involves multiple structs that conform to a protocol, and it performs calculations on an array of shapes. The code is also well-commented, which makes it easier to understand and maintain.