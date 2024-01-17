```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape interface
@protocol Shape

- (CGFloat)area;
- (CGFloat)perimeter;

@end

// Define a class for a Rectangle that conforms to the Shape protocol
@interface Rectangle : NSObject <Shape>

// Private instance variables
@private
CGFloat width;
CGFloat height;

// Public methods
- (instancetype)initWithWidth:(CGFloat)width height:(CGFloat)height;
- (CGFloat)area;
- (CGFloat)perimeter;

@end

// Define a class for a Circle that conforms to the Shape protocol
@interface Circle : NSObject <Shape>

// Private instance variables
@private
CGFloat radius;

// Public methods
- (instancetype)initWithRadius:(CGFloat)radius;
- (CGFloat)area;
- (CGFloat)perimeter;

@end

// Define a class for a ShapeCalculator that performs calculations on shapes
@interface ShapeCalculator : NSObject

// Public methods
+ (CGFloat)totalAreaForShapes:(NSArray<Shape *> *)shapes;
+ (CGFloat)totalPerimeterForShapes:(NSArray<Shape *> *)shapes;

@end

// Implementation of the Rectangle class
@implementation Rectangle

- (instancetype)initWithWidth:(CGFloat)width height:(CGFloat)height {
    self = [super init];
    if (self) {
        self.width = width;
        self.height = height;
    }
    return self;
}

- (CGFloat)area {
    return self.width * self.height;
}

- (CGFloat)perimeter {
    return 2 * (self.width + self.height);
}

@end

// Implementation of the Circle class
@implementation Circle

- (instancetype)initWithRadius:(CGFloat)radius {
    self = [super init];
    if (self) {
        self.radius = radius;
    }
    return self;
}

- (CGFloat)area {
    return M_PI * self.radius * self.radius;
}

- (CGFloat)perimeter {
    return 2 * M_PI * self.radius;
}

@end

// Implementation of the ShapeCalculator class
@implementation ShapeCalculator

+ (CGFloat)totalAreaForShapes:(NSArray<Shape *> *)shapes {
    CGFloat totalArea = 0;
    for (Shape *shape in shapes) {
        totalArea += [shape area];
    }
    return totalArea;
}

+ (CGFloat)totalPerimeterForShapes:(NSArray<Shape *> *)shapes {
    CGFloat totalPerimeter = 0;
    for (Shape *shape in shapes) {
        totalPerimeter += [shape perimeter];
    }
    return totalPerimeter;
}

@end

// Main function to test the classes and methods
int main(int argc, const char *argv[]) {
    @autoreleasepool {
        // Create an array of shapes
        NSArray<Shape *> *shapes = @[
            [[Rectangle alloc] initWithWidth:5 height:10],
            [[Circle alloc] initWithRadius:3],
            [[Rectangle alloc] initWithWidth:2 height:4],
            [[Circle alloc] initWithRadius:1]
        ];

        // Calculate the total area and perimeter of the shapes
        CGFloat totalArea = [ShapeCalculator totalAreaForShapes:shapes];
        CGFloat totalPerimeter = [ShapeCalculator totalPerimeterForShapes:shapes];

        // Print the results
        NSLog(@"Total area: %.2f", totalArea);
        NSLog(@"Total perimeter: %.2f", totalPerimeter);
    }
    return 0;
}
```

This code demonstrates some advanced concepts in Objective-C, including protocols, classes, inheritance, polymorphism, and dynamic dispatch. Here's an explanation of the code:

* **Shape Protocol**: The code defines a `Shape` protocol that declares two methods: `area` and `perimeter`. This protocol defines the interface for any object that can represent a shape, and it allows different shapes to be treated uniformly.

* **Rectangle and Circle Classes**: The `Rectangle` and `Circle` classes conform to the `Shape` protocol. They implement the `area` and `perimeter` methods to calculate the area and perimeter of the respective shapes.

* **ShapeCalculator Class**: The `ShapeCalculator` class provides utility methods to calculate the total area and perimeter of an array of `Shape` objects. These methods use polymorphism to handle different types of shapes uniformly.

* **Main Function**: The `main` function creates an array of shape objects, including rectangles and circles. It then uses the `ShapeCalculator` class to calculate the total area and perimeter of these shapes and prints the results.

This code showcases how to define protocols, create classes that conform to those protocols, and use polymorphism to perform calculations on different types of objects in an elegant and type-safe manner.