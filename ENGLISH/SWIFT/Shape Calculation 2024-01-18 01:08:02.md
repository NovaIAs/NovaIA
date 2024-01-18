```swift
// Define a protocol for a Shape object.
protocol Shape {
    func area() -> Double
    func perimeter() -> Double
}

// Define a class for a Rectangle object that conforms to the Shape protocol.
class Rectangle: Shape {
    var width: Double
    var height: Double

    init(width: Double, height: Double) {
        self.width = width
        self.height = height
    }

    func area() -> Double {
        return width * height
    }

    func perimeter() -> Double {
        return 2 * width + 2 * height
    }
}

// Define a class for a Circle object that conforms to the Shape protocol.
class Circle: Shape {
    var radius: Double

    init(radius: Double) {
        self.radius = radius
    }

    func area() -> Double {
        return Double.pi * radius * radius
    }

    func perimeter() -> Double {
        return 2 * Double.pi * radius
    }
}

// Define a class for a Triangle object that conforms to the Shape protocol.
class Triangle: Shape {
    var base: Double
    var height: Double

    init(base: Double, height: Double) {
        self.base = base
        self.height = height
    }

    func area() -> Double {
        return 0.5 * base * height
    }

    func perimeter() -> Double {
        let side1 = sqrt(base * base + height * height)
        let side2 = sqrt(base * base + height * height)
        return base + side1 + side2
    }
}

// Create an array of Shape objects.
let shapes: [Shape] = [
    Rectangle(width: 5.0, height: 10.0),
    Circle(radius: 5.0),
    Triangle(base: 5.0, height: 10.0)
]

// Iterate over the array and calculate the area and perimeter of each Shape object.
for shape in shapes {
    print("Area: \(shape.area()), Perimeter: \(shape.perimeter())")
}
```

Explanation:

This code defines a protocol called `Shape` with two methods: `area()` and `perimeter()`. These methods are used to calculate the area and perimeter of a shape, respectively.

Next, three classes are defined: `Rectangle`, `Circle`, and `Triangle`. These classes all conform to the `Shape` protocol, meaning that they must implement the `area()` and `perimeter()` methods. The `Rectangle` class has two properties: `width` and `height`. The `Circle` class has one property: `radius`. The `Triangle` class has two properties: `base` and `height`.

Each class has an initializer that initializes its properties. The `area()` and `perimeter()` methods are implemented for each class, using the appropriate formulas to calculate the area and perimeter of the shape.

Finally, an array of `Shape` objects is created, containing a rectangle, a circle, and a triangle. A loop is then used to iterate over the array and calculate the area and perimeter of each shape. The results are printed to the console.