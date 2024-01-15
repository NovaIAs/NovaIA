```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a shape object
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a base Shape class
@interface Shape : NSObject <Shape>
- (instancetype)initWithWidth:(double)width height:(double)height;
@property (nonatomic, readonly) double width;
@property (nonatomic, readonly) double height;
@end

@implementation Shape
- (instancetype)initWithWidth:(double)width height:(double)height {
    self = [super init];
    if (self) {
        _width = width;
        _height = height;
    }
    return self;
}

- (double)area {
    return 0.0;
}

- (double)perimeter {
    return 0.0;
}
@end

// Define a subclass of Shape for a rectangle
@interface Rectangle : Shape
@end

@implementation Rectangle
- (double)area {
    return self.width * self.height;
}

- (double)perimeter {
    return 2 * (self.width + self.height);
}
@end

// Define a subclass of Shape for a triangle
@interface Triangle : Shape
@end

@implementation Triangle
- (double)area {
    return 0.5 * self.width * self.height;
}

- (double)perimeter {
    double hypotenuse = sqrt(pow(self.width, 2) + pow(self.height, 2));
    return self.width + self.height + hypotenuse;
}
@end

// Define a subclass of Shape for a circle
@interface Circle : Shape
@end

@implementation Circle
- (double)area {
    return M_PI * pow(self.width, 2);
}

- (double)perimeter {
    return 2 * M_PI * self.width;
}
@end

// Define a function to calculate the total area of an array of shapes
double calculateTotalArea(NSArray<Shape *> *shapes) {
    double totalArea = 0.0;
    for (Shape *shape in shapes) {
        totalArea += [shape area];
    }
    return totalArea;
}

// Define a function to calculate the total perimeter of an array of shapes
double calculateTotalPerimeter(NSArray<Shape *> *shapes) {
    double totalPerimeter = 0.0;
    for (Shape *shape in shapes) {
        totalPerimeter += [shape perimeter];
    }
    return totalPerimeter;
}

// Test the code
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an array of shapes
        NSArray<Shape *> *shapes = @[
            [[Rectangle alloc] initWithWidth:10.0 height:5.0],
            [[Triangle alloc] initWithWidth:10.0 height:5.0],
            [[Circle alloc] initWithWidth:5.0]
        ];

        // Calculate the total area and perimeter of the shapes
        double totalArea = calculateTotalArea(shapes);
        double totalPerimeter = calculateTotalPerimeter(shapes);

        // Print the results
        NSLog(@"Total area: %.2f", totalArea);
        NSLog(@"Total perimeter: %.2f", totalPerimeter);
    }
    return 0;
}
```

Explanation:

1. We define a protocol called `Shape` that declares two methods: `area` and `perimeter`. This protocol defines the common interface that all shape objects must implement.

2. We define a base class called `Shape` that conforms to the `Shape` protocol. This class provides a common implementation for properties and methods that are common to all shapes, such as the width and height properties.

3. We define three subclasses of `Shape`: `Rectangle`, `Triangle`, and `Circle`. These classes implement the `area` and `perimeter` methods specific to each shape.

4. We define two functions, `calculateTotalArea` and `calculateTotalPerimeter`, to calculate the total area and perimeter of an array of shapes, respectively.

5. In the `main` function, we create an array of shapes containing a rectangle, a triangle, and a circle. We then call the `calculateTotalArea` and `calculateTotalPerimeter` functions to calculate the total area and perimeter of the shapes. Finally, we print the results to the console.

This code demonstrates object-oriented programming principles, such as inheritance, polymorphism, and protocols, to create a flexible and extensible framework for calculating the area and perimeter of different shapes.