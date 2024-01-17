```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape object
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a class for a Rectangle object
@interface Rectangle : NSObject <Shape>
@property double width;
@property double height;
- (instancetype)initWithWidth:(double)width height:(double)height;
@end

@implementation Rectangle
- (instancetype)initWithWidth:(double)width height:(double)height {
    self = [super init];
    if (self) {
        self.width = width;
        self.height = height;
    }
    return self;
}

- (double)area {
    return self.width * self.height;
}

- (double)perimeter {
    return 2 * (self.width + self.height);
}
@end

// Define a class for a Circle object
@interface Circle : NSObject <Shape>
@property double radius;
- (instancetype)initWithRadius:(double)radius;
@end

@implementation Circle
- (instancetype)initWithRadius:(double)radius {
    self = [super init];
    if (self) {
        self.radius = radius;
    }
    return self;
}

- (double)area {
    return M_PI * self.radius * self.radius;
}

- (double)perimeter {
    return 2 * M_PI * self.radius;
}
@end

// Define a function to calculate the total area of an array of shapes
double calculateTotalArea(NSArray<Shape *> *shapes) {
    double totalArea = 0;
    for (Shape *shape in shapes) {
        totalArea += [shape area];
    }
    return totalArea;
}

// Define a function to calculate the total perimeter of an array of shapes
double calculateTotalPerimeter(NSArray<Shape *> *shapes) {
    double totalPerimeter = 0;
    for (Shape *shape in shapes) {
        totalPerimeter += [shape perimeter];
    }
    return totalPerimeter;
}

// Main function
int main() {
    // Create an array of shapes
    NSArray<Shape *> *shapes = @[
        [[Rectangle alloc] initWithWidth:10 height:5],
        [[Circle alloc] initWithRadius:3],
        [[Rectangle alloc] initWithWidth:7 height:8],
        [[Circle alloc] initWithRadius:4]
    ];

    // Calculate the total area and perimeter of the shapes
    double totalArea = calculateTotalArea(shapes);
    double totalPerimeter = calculateTotalPerimeter(shapes);

    // Print the results
    NSLog(@"Total area: %.2f", totalArea);
    NSLog(@"Total perimeter: %.2f", totalPerimeter);

    return 0;
}
```
Explanation:

1. We define a protocol called `Shape` with two methods: `area` and `perimeter`. This protocol defines the common interface that all shape objects must implement.


2. We define two classes, `Rectangle` and `Circle`, which conform to the `Shape` protocol. Each class has its own properties and methods to calculate the area and perimeter of the shape.


3. We define a function called `calculateTotalArea` to calculate the total area of an array of shapes. It iterates over the array, calling the `area` method on each shape and accumulating the results.


4. We define a function called `calculateTotalPerimeter` to calculate the total perimeter of an array of shapes. It iterates over the array, calling the `perimeter` method on each shape and accumulating the results.


5. In the `main` function, we create an array of shape objects, including rectangles and circles.


6. We call the `calculateTotalArea` and `calculateTotalPerimeter` functions to calculate the total area and perimeter of the shapes.


7. We print the results to the console.

This code demonstrates how to define and use protocols in Objective-C to create a common interface for different types of objects. It also shows how to use functions to perform operations on arrays of objects.