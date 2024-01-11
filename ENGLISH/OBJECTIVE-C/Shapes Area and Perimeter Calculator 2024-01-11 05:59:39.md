```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for a Shape
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a class for a Rectangle
@interface Rectangle : NSObject <Shape>
{
    double width;
    double height;
}

- (id)initWithWidth:(double)w andHeight:(double)h;
- (double)area;
- (double)perimeter;

@end

// Define a class for a Circle
@interface Circle : NSObject <Shape>
{
    double radius;
}

- (id)initWithRadius:(double)r;
- (double)area;
- (double)perimeter;

@end

// Define a class for a Triangle
@interface Triangle : NSObject <Shape>
{
    double sideA;
    double sideB;
    double sideC;
}

- (id)initWithSideA:(double)a andSideB:(double)b andSideC:(double)c;
- (double)area;
- (double)perimeter;

@end

// Implementation of the Rectangle class
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

// Implementation of the Circle class
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

// Implementation of the Triangle class
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

// Main function
int main()
{
    // Create an array of shapes
    NSMutableArray *shapes = [NSMutableArray array];

    // Add a rectangle to the array
    Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10 andHeight:5];
    [shapes addObject:rectangle];

    // Add a circle to the array
    Circle *circle = [[Circle alloc] initWithRadius:5];
    [shapes addObject:circle];

    // Add a triangle to the array
    Triangle *triangle = [[Triangle alloc] initWithSideA:3 andSideB:4 andSideC:5];
    [shapes addObject:triangle];

    // Print the area and perimeter of each shape
    for (id shape in shapes) {
        NSLog(@"Shape: %@", [shape class]);
        NSLog(@"Area: %.2f", [shape area]);
        NSLog(@"Perimeter: %.2f", [shape perimeter]);
        NSLog(@"");
    }

    return 0;
}
```

This code defines three classes: `Rectangle`, `Circle`, and `Triangle`, which all conform to the `Shape` protocol. The `Shape` protocol defines two methods: `area` and `perimeter`, which return the area and perimeter of the shape, respectively.

The `Rectangle` class has two instance variables, `width` and `height`, and it implements the `area` and `perimeter` methods to calculate the area and perimeter of a rectangle.

The `Circle` class has one instance variable, `radius`, and it implements the `area` and `perimeter` methods to calculate the area and perimeter of a circle.

The `Triangle` class has three instance variables, `sideA`, `sideB`, and `sideC`, and it implements the `area` and `perimeter` methods to calculate the area and perimeter of a triangle.

The `main` function creates an array of shapes, adds a rectangle, a circle, and a triangle to the array, and then prints the area and perimeter of each shape.