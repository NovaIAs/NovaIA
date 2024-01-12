```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a class for a Rectangle that conforms to the Shape protocol
@interface Rectangle : NSObject <Shape>
{
    double width;
    double height;
}

- (id)initWithWidth:(double)w andHeight:(double)h;
- (double)area;
- (double)perimeter;
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

// Define a class for a Circle that conforms to the Shape protocol
@interface Circle : NSObject <Shape>
{
    double radius;
}

- (id)initWithRadius:(double)r;
- (double)area;
- (double)perimeter;
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

// Define a class for a ShapeCalculator
@interface ShapeCalculator : NSObject
{
    NSMutableArray *shapes;
}

- (id)init;
- (void)addShape:(Shape *)shape;
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

- (void)addShape:(Shape *)shape
{
    [shapes addObject:shape];
}

- (double)totalArea
{
    double totalArea = 0;
    for (Shape *shape in shapes) {
        totalArea += [shape area];
    }
    return totalArea;
}

- (double)totalPerimeter
{
    double totalPerimeter = 0;
    for (Shape *shape in shapes) {
        totalPerimeter += [shape perimeter];
    }
    return totalPerimeter;
}
@end

// Main program
int main(int argc, const char * argv[])
{
    @autoreleasepool {
        // Create a ShapeCalculator object
        ShapeCalculator *calculator = [[ShapeCalculator alloc] init];

        // Create a Rectangle object and add it to the calculator
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10 andHeight:5];
        [calculator addShape:rectangle];

        // Create a Circle object and add it to the calculator
        Circle *circle = [[Circle alloc] initWithRadius:5];
        [calculator addShape:circle];

        // Calculate the total area and perimeter of the shapes
        double totalArea = [calculator totalArea];
        double totalPerimeter = [calculator totalPerimeter];

        // Print the results
        NSLog(@"Total area: %.2f", totalArea);
        NSLog(@"Total perimeter: %.2f", totalPerimeter);
    }
    return 0;
}
```

This code defines a protocol called `Shape` with two required methods: `area` and `perimeter`. It then defines two classes, `Rectangle` and `Circle`, that conform to the `Shape` protocol. These classes provide implementations for the `area` and `perimeter` methods for rectangles and circles, respectively.

The code also defines a class called `ShapeCalculator` that maintains a collection of shapes and provides methods for calculating the total area and perimeter of the shapes in the collection.

The main program creates a `ShapeCalculator` object and adds a `Rectangle` and a `Circle` to it. It then calls the `totalArea` and `totalPerimeter` methods to calculate the total area and perimeter of the shapes in the collection. The results are then printed to the console.

This code demonstrates object-oriented programming principles such as polymorphism, inheritance, and encapsulation. It also shows how to use protocols to define a common interface for different classes.