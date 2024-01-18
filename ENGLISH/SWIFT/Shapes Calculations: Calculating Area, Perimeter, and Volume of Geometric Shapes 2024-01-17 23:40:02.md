```swift
// MARK: - Protocol Declarations

protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

protocol ThreeDimensionalShape: Shape {
    func volume() -> Double
}

// MARK: - Struct Declarations

struct Circle: Shape {
    var radius: Double

    func area() -> Double {
        return Double.pi * radius * radius
    }

    func perimeter() -> Double {
        return 2.0 * Double.pi * radius
    }
}

struct Square: Shape {
    var sideLength: Double

    func area() -> Double {
        return sideLength * sideLength
    }

    func perimeter() -> Double {
        return 4.0 * sideLength
    }
}

struct Cube: ThreeDimensionalShape {
    var edgeLength: Double

    func area() -> Double {
        return 6.0 * edgeLength * edgeLength
    }

    func perimeter() -> Double {
        return 12.0 * edgeLength
    }

    func volume() -> Double {
        return edgeLength * edgeLength * edgeLength
    }
}

// MARK: - Function Declarations

func calculateShapesArea(shapes: [Shape]) -> Double {
    return shapes.reduce(0.0, { (totalArea, shape) -> Double in
        return totalArea + shape.area()
    })
}

func calculateShapesPerimeter(shapes: [Shape]) -> Double {
    return shapes.reduce(0.0, { (totalPerimeter, shape) -> Double in
        return totalPerimeter + shape.perimeter()
    })
}

func calculateShapesVolume(shapes: [ThreeDimensionalShape]) -> Double {
    return shapes.reduce(0.0, { (totalVolume, shape) -> Double in
        return totalVolume + shape.volume()
    })
}

// MARK: - Main Execution

let circle = Circle(radius: 5.0)
let square = Square(sideLength: 10.0)
let cube = Cube(edgeLength: 3.0)

let shapes: [Shape] = [circle, square]
let threeDimensionalShapes: [ThreeDimensionalShape] = [cube]

let totalArea = calculateShapesArea(shapes: shapes)
let totalPerimeter = calculateShapesPerimeter(shapes: shapes)

let totalVolume = calculateShapesVolume(shapes: threeDimensionalShapes)

print("Total Area: \(totalArea)")
print("Total Perimeter: \(totalPerimeter)")
print("Total Volume: \(totalVolume)")
```

Explanation:

1. **Protocol Declarations**:
   - `Shape`: Defines the common interface for calculating the area and perimeter of a shape.
   - `ThreeDimensionalShape`: Inherits from `Shape` and adds the ability to calculate the volume of a 3D shape.

2. **Struct Declarations**:
   - `Circle`, `Square`, and `Cube` are structs that conform to the `Shape` protocol and/or the `ThreeDimensionalShape` protocol.
   - Each struct has its own implementation of `area()`, `perimeter()`, and (for `Cube`) `volume()` methods.

3. **Function Declarations**:
   - `calculateShapesArea`, `calculateShapesPerimeter`, and `calculateShapesVolume` are functions that take an array of shapes and calculate the total area, perimeter, or volume, respectively.

4. **Main Execution**:
   - Creates instances of `Circle`, `Square`, and `Cube`.
   - Creates an array of `Shape` objects and an array of `ThreeDimensionalShape` objects.
   - Calls the helper functions to calculate the total area, perimeter, and volume of the shapes.
   - Prints the results to the console.

This code demonstrates the concept of protocols and structs in Swift, as well as how to define and use custom functions to perform calculations on objects that conform to a specific protocol. It also showcases the use of generics with arrays of shapes and the `reduce()` method for performing cumulative calculations.