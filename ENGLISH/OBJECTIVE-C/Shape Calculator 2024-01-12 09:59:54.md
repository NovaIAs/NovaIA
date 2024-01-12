```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a class for a Circle
@interface Circle : NSObject <Shape>
{
    double radius;
}

- (id)initWithRadius:(double)r;
@end

@implementation Circle

- (id)initWithRadius:(double)r
{
    self = [super init];
    if (self) {
        radius = r;
    }
    return self;
}

- (double)area
{
    return M_PI * radius * radius;
}

- (double)perimeter
{
    return 2 * M_PI * radius;
}

@end

// Define a class for a Rectangle
@interface Rectangle : NSObject <Shape>
{
    double width;
    double height;
}

- (id)initWithWidth:(double)w andHeight:(double)h;
@end

@implementation Rectangle

- (id)initWithWidth:(double)w andHeight:(double)h
{
    self = [super init];
    if (self) {
        width = w;
        height = h;
    }
    return self;
}

- (double)area
{
    return width * height;
}

- (double)perimeter
{
    return 2 * (width + height);
}

@end

// Define a class for a Triangle
@interface Triangle : NSObject <Shape>
{
    double sideA;
    double sideB;
    double sideC;
}

- (id)initWithSideA:(double)a andSideB:(double)b andSideC:(double)c;
@end

@implementation Triangle

- (id)initWithSideA:(double)a andSideB:(double)b andSideC:(double)c
{
    self = [super init];
    if (self) {
        sideA = a;
        sideB = b;
        sideC = c;
    }
    return self;
}

- (double)area
{
    double s = (sideA + sideB + sideC) / 2;
    return sqrt(s * (s - sideA) * (s - sideB) * (s - sideC));
}

- (double)perimeter
{
    return sideA + sideB + sideC;
}

@end

// Define a class for a ShapeCalculator
@interface ShapeCalculator : NSObject
{
    NSMutableArray *shapes;
}

- (id)init;
- (void)addShape:(id<Shape>)shape;
- (double)totalArea;
- (double)totalPerimeter;
@end

@implementation ShapeCalculator

- (id)init
{
    self = [super init];
    if (self) {
        shapes = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addShape:(id<Shape>)shape
{
    [shapes addObject:shape];
}

- (double)totalArea
{
    double total = 0;
    for (id<Shape> shape in shapes) {
        total += [shape area];
    }
    return total;
}

- (double)totalPerimeter
{
    double total = 0;
    for (id<Shape> shape in shapes) {
        total += [shape perimeter];
    }
    return total;
}

@end

// Main function
int main(int argc, char *argv[])
{
    @autoreleasepool {
        // Create a shape calculator
        ShapeCalculator *calculator = [[ShapeCalculator alloc] init];

        // Create a circle
        Circle *circle = [[Circle alloc] initWithRadius:5.0];

        // Create a rectangle
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10.0 andHeight:20.0];

        // Create a triangle
        Triangle *triangle = [[Triangle alloc] initWithSideA:3.0 andSideB:4.0 andSideC:5.0];

        // Add the shapes to the calculator
        [calculator addShape:circle];
        [calculator addShape:rectangle];
        [calculator addShape:triangle];

        // Print the total area and perimeter
        NSLog(@"Total area: %f", [calculator totalArea]);
        NSLog(@"Total perimeter: %f", [calculator totalPerimeter]);
    }
    return 0;
}
```

This code demonstrates the use of protocols, classes, and inheritance in Objective-C to create a program that calculates the total area and perimeter of a collection of shapes.

The `Shape` protocol defines two methods that all shapes must implement: `area` and `perimeter`.

The `Circle`, `Rectangle`, and `Triangle` classes all conform to the `Shape` protocol and provide implementations for the `area` and `perimeter` methods.

The `ShapeCalculator` class maintains a collection of shapes and provides methods to calculate the total area and perimeter of the shapes in the collection.

The `main` function creates a `ShapeCalculator` object and adds a `Circle`, `Rectangle`, and `Triangle` to the calculator. It then prints the total area and perimeter of the shapes in the calculator.