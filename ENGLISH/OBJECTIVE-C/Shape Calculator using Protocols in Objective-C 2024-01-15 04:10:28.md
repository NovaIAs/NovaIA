```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape interface
@protocol Shape

- (double)area;
- (double)perimeter;

@end

// Define a class for a Rectangle
@interface Rectangle : NSObject <Shape>

@property (nonatomic) double width;
@property (nonatomic) double height;

- (instancetype)initWithWidth:(double)width height:(double)height;

@end

// Define a class for a Circle
@interface Circle : NSObject <Shape>

@property (nonatomic) double radius;

- (instancetype)initWithRadius:(double)radius;

@end

// Define a class for a Triangle
@interface Triangle : NSObject <Shape>

@property (nonatomic) double sideA;
@property (nonatomic) double sideB;
@property (nonatomic) double sideC;

- (instancetype)initWithSideA:(double)sideA sideB:(double)sideB sideC:(double)sideC;

@end

// Implement the Shape protocol for Rectangle
@implementation Rectangle

- (instancetype)initWithWidth:(double)width height:(double)height {
    self = [super init];
    if (self) {
        _width = width;
        _height = height;
    }
    return self;
}

- (double)area {
    return _width * _height;
}

- (double)perimeter {
    return 2 * (_width + _height);
}

@end

// Implement the Shape protocol for Circle
@implementation Circle

- (instancetype)initWithRadius:(double)radius {
    self = [super init];
    if (self) {
        _radius = radius;
    }
    return self;
}

- (double)area {
    return M_PI * _radius * _radius;
}

- (double)perimeter {
    return 2 * M_PI * _radius;
}

@end

// Implement the Shape protocol for Triangle
@implementation Triangle

- (instancetype)initWithSideA:(double)sideA sideB:(double)sideB sideC:(double)sideC {
    self = [super init];
    if (self) {
        _sideA = sideA;
        _sideB = sideB;
        _sideC = sideC;
    }
    return self;
}

- (double)area {
    double semiperimeter = (_sideA + _sideB + _sideC) / 2;
    return sqrt(semiperimeter * (semiperimeter - _sideA) * (semiperimeter - _sideB) * (semiperimeter - _sideC));
}

- (double)perimeter {
    return _sideA + _sideB + _sideC;
}

@end

// Test the Shape protocol implementations
int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Create instances of Rectangle, Circle, and Triangle
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10 height:5];
        Circle *circle = [[Circle alloc] initWithRadius:5];
        Triangle *triangle = [[Triangle alloc] initWithSideA:3 sideB:4 sideC:5];

        // Print the area and perimeter of each shape
        NSLog(@"Rectangle: Area = %.2f, Perimeter = %.2f", [rectangle area], [rectangle perimeter]);
        NSLog(@"Circle: Area = %.2f, Perimeter = %.2f", [circle area], [circle perimeter]);
        NSLog(@"Triangle: Area = %.2f, Perimeter = %.2f", [triangle area], [triangle perimeter]);
    }
    return 0;
}
```

Explanation:

1. Define a protocol `Shape` with two required methods: `area` and `perimeter`, which calculate the area and perimeter of a shape, respectively.

2. Define three classes: `Rectangle`, `Circle`, and `Triangle`, each implementing the `Shape` protocol.

3. In each class, define properties to store the dimensions of the shape and implement the `area` and `perimeter` methods to calculate the area and perimeter using the appropriate formulas.

4. In the `main` function:
   - Create instances of `Rectangle`, `Circle`, and `Triangle` with specific dimensions.
   - Call the `area` and `perimeter` methods on each shape and print the results to the console.

This code demonstrates object-oriented programming principles, including protocols, interfaces, and polymorphism. It illustrates how to define a common interface for different types of objects and implement specific behaviors in each class. The use of protocols enables you to work with different shapes consistently, regardless of their specific implementation details.