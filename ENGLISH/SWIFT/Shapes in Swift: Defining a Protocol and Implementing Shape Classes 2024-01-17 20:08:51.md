```swift
// Define a protocol that represents a Shape

protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

// Define a class that represents a Circle

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

// Define a class that represents a Rectangle

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
        return 2 * (width + height)
    }
}

// Define a class that represents a Triangle

class Triangle: Shape {
    var sideA: Double
    var sideB: Double
    var sideC: Double

    init(sideA: Double, sideB: Double, sideC: Double) {
        self.sideA = sideA
        self.sideB = sideB
        self.sideC = sideC
    }

    var area: Double {
        let s = (sideA + sideB + sideC) / 2
        return sqrt(s * (s - sideA) * (s - sideB) * (s - sideC))
    }

    var perimeter: Double {
        return sideA + sideB + sideC
    }
}

// Create an array of shapes

let shapes: [Shape] = [
    Circle(radius: 5),
    Rectangle(width: 10, height: 15),
    Triangle(sideA: 3, sideB: 4, sideC: 5)
]

// Print the area and perimeter of each shape

for shape in shapes {
    print("Area: \(shape.area)")
    print("Perimeter: \(shape.perimeter)")
}
```

**Explanation:**

* The `Shape` protocol defines the interface for all shapes. It has two properties: `area` and `perimeter`.
* The `Circle`, `Rectangle`, and `Triangle` classes implement the `Shape` protocol. Each class has its own implementation of the `area` and `perimeter` properties.
* The `shapes` array contains instances of the `Circle`, `Rectangle`, and `Triangle` classes.
* The `for` loop iterates through the `shapes` array and prints the area and perimeter of each shape.

This code demonstrates the use of protocols, inheritance, and polymorphism in Swift.