```swift
import Foundation

// Define a protocol for a Shape
protocol Shape {
    var area: Double { get }
    var perimeter: Double { get }
}

// Define a struct for a Circle
struct Circle: Shape {
    var radius: Double
    
    var area: Double {
        return Double.pi * radius * radius
    }
    
    var perimeter: Double {
        return 2 * Double.pi * radius
    }
}

// Define a struct for a Rectangle
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

// Define a struct for a Square
struct Square: Shape {
    var sideLength: Double
    
    var area: Double {
        return sideLength * sideLength
    }
    
    var perimeter: Double {
        return 4 * sideLength
    }
}

// Define a function to calculate the total area of an array of shapes
func totalArea(shapes: [Shape]) -> Double {
    var total: Double = 0
    
    for shape in shapes {
        total += shape.area
    }
    
    return total
}

// Define a function to calculate the total perimeter of an array of shapes
func totalPerimeter(shapes: [Shape]) -> Double {
    var total: Double = 0
    
    for shape in shapes {
        total += shape.perimeter
    }
    
    return total
}

// Example usage
let circle = Circle(radius: 5.0)
let rectangle = Rectangle(width: 10.0, height: 20.0)
let square = Square(sideLength: 7.0)

let shapes: [Shape] = [circle, rectangle, square]

print("Total area:", totalArea(shapes: shapes))
print("Total perimeter:", totalPerimeter(shapes: shapes))
```

Explanation:

1. We define a Shape protocol to establish a common interface for all shapes.

2. We define three structs: Circle, Rectangle, and Square, all of which conform to the Shape protocol. Each struct implements the area and perimeter properties and provides a method to calculate these values.

3. We define two functions: totalArea and totalPerimeter to calculate the total area and total perimeter of an array of shapes, respectively.

4. We create an array of shapes called shapes, which includes a circle, a rectangle, and a square.

5. We use the totalArea and totalPerimeter functions to calculate the total area and total perimeter of the shapes array and print the results.