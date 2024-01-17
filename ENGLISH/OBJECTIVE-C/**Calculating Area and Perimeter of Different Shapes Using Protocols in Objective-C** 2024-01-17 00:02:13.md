``` objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a shape
@protocol Shape

// Method to calculate the area of the shape
- (CGFloat)area;

// Method to calculate the perimeter of the shape
- (CGFloat)perimeter;

@end

// Define a class for a rectangle
@interface Rectangle : NSObject <Shape>

// Instance variables for the rectangle
@property CGFloat width;
@property CGFloat height;

// Method to initialize the rectangle with a width and height
- (instancetype)initWithWidth:(CGFloat)width height:(CGFloat)height;

// Method to calculate the area of the rectangle
- (CGFloat)area;

// Method to calculate the perimeter of the rectangle
- (CGFloat)perimeter;

@end

// Define a class for a circle
@interface Circle : NSObject <Shape>

// Instance variable for the radius of the circle
@property CGFloat radius;

// Method to initialize the circle with a radius
- (instancetype)initWithRadius:(CGFloat)radius;

// Method to calculate the area of the circle
- (CGFloat)area;

// Method to calculate the perimeter of the circle
- (CGFloat)perimeter;

@end

// Define a class for a triangle
@interface Triangle : NSObject <Shape>

// Instance variables for the sides of the triangle
@property CGFloat sideA;
@property CGFloat sideB;
@property CGFloat sideC;

// Method to initialize the triangle with the lengths of its sides
- (instancetype)initWithSideA:(CGFloat)sideA sideB:(CGFloat)sideB sideC:(CGFloat)sideC;

// Method to calculate the area of the triangle
- (CGFloat)area;

// Method to calculate the perimeter of the triangle
- (CGFloat)perimeter;

@end

// Main function
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an array of shapes
        NSMutableArray *shapes = [NSMutableArray array];

        // Create a rectangle with a width of 10 and a height of 5
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10 height:5];

        // Create a circle with a radius of 5
        Circle *circle = [[Circle alloc] initWithRadius:5];

        // Create a triangle with sides of length 3, 4, and 5
        Triangle *triangle = [[Triangle alloc] initWithSideA:3 sideB:4 sideC:5];

        // Add the shapes to the array
        [shapes addObject:rectangle];
        [shapes addObject:circle];
        [shapes addObject:triangle];

        // Calculate the total area and perimeter of the shapes
        CGFloat totalArea = 0;
        CGFloat totalPerimeter = 0;

        for (Shape *shape in shapes) {
            totalArea += [shape area];
            totalPerimeter += [shape perimeter];
        }

        // Print the total area and perimeter
        NSLog(@"Total area: %.2f", totalArea);
        NSLog(@"Total perimeter: %.2f", totalPerimeter);
    }
    return 0;
}

// Implementation of the Rectangle class

@implementation Rectangle

// Method to initialize the rectangle with a width and height
- (instancetype)initWithWidth:(CGFloat)width height:(CGFloat)height {
    self = [super init];
    if (self) {
        _width = width;
        _height = height;
    }
    return self;
}

// Method to calculate the area of the rectangle
- (CGFloat)area {
    return _width * _height;
}

// Method to calculate the perimeter of the rectangle
- (CGFloat)perimeter {
    return 2 * (_width + _height);
}

@end

// Implementation of the Circle class

@implementation Circle

// Method to initialize the circle with a radius
- (instancetype)initWithRadius:(CGFloat)radius {
    self = [super init];
    if (self) {
        _radius = radius;
    }
    return self;
}

// Method to calculate the area of the circle
- (CGFloat)area {
    return M_PI * _radius * _radius;
}

// Method to calculate the perimeter of the circle
- (CGFloat)perimeter {
    return 2 * M_PI * _radius;
}

@end

// Implementation of the Triangle class

@implementation Triangle

// Method to initialize the triangle with the lengths of its sides
- (instancetype)initWithSideA:(CGFloat)sideA sideB:(CGFloat)sideB sideC:(CGFloat)sideC {
    self = [super init];
    if (self) {
        _sideA = sideA;
        _sideB = sideB;
        _sideC = sideC;
    }
    return self;
}

// Method to calculate the area of the triangle
- (CGFloat)area {
    CGFloat semiperimeter = (_sideA + _sideB + _sideC) / 2;
    return sqrt(semiperimeter * (semiperimeter - _sideA) * (semiperimeter - _sideB) * (semiperimeter - _sideC));
}

// Method to calculate the perimeter of the triangle
- (CGFloat)perimeter {
    return _sideA + _sideB + _sideC;
}

@end
```

This code defines three classes: Rectangle, Circle, and Triangle, which all conform to the Shape protocol. The Shape protocol defines two methods: area and perimeter. The Rectangle, Circle, and Triangle classes implement these methods to calculate the area and perimeter of their respective shapes.

The main function creates an array of shapes and adds a rectangle, a circle, and a triangle to the array. It then calculates the total area and perimeter of the shapes and prints the results.

This code is complex because it involves multiple classes, protocols, and methods. It also uses some advanced mathematics to calculate the area of the triangle. However, the code is well-organized and easy to understand.