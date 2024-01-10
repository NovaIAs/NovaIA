```objective-c
// Import necessary header files
#import <Foundation/Foundation.h>

// Define a macro for convenience
#define PI 3.14159265

// Declare a protocol for shapes
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Implement the Shape protocol for a circle
@interface Circle : NSObject <Shape>
{
    double _radius;
}

- (instancetype)initWithRadius:(double)radius;
- (double)area;
- (double)perimeter;
@end

// Implement the Circle class
@implementation Circle

- (instancetype)initWithRadius:(double)radius
{
    self = [super init];
    if (self) {
        _radius = radius;
    }
    return self;
}

- (double)area
{
    return PI * _radius * _radius;
}

- (double)perimeter
{
    return 2 * PI * _radius;
}

@end

// Implement the Shape protocol for a rectangle
@interface Rectangle : NSObject <Shape>
{
    double _width;
    double _height;
}

- (instancetype)initWithWidth:(double)width height:(double)height;
- (double)area;
- (double)perimeter;
@end

// Implement the Rectangle class
@implementation Rectangle

- (instancetype)initWithWidth:(double)width height:(double)height
{
    self = [super init];
    if (self) {
        _width = width;
        _height = height;
    }
    return self;
}

- (double)area
{
    return _width * _height;
}

- (double)perimeter
{
    return 2 * (_width + _height);
}

@end

// Implement the Shape protocol for a triangle
@interface Triangle : NSObject <Shape>
{
    double _base;
    double _height;
    double _side1;
    double _side2;
}

- (instancetype)initWithBase:(double)base height:(double)height side1:(double)side1 side2:(double)side2;
- (double)area;
- (double)perimeter;
@end

// Implement the Triangle class
@implementation Triangle

- (instancetype)initWithBase:(double)base height:(double)height side1:(double)side1 side2:(double)side2
{
    self = [super init];
    if (self) {
        _base = base;
        _height = height;
        _side1 = side1;
        _side2 = side2;
    }
    return self;
}

- (double)area
{
    return 0.5 * _base * _height;
}

- (double)perimeter
{
    return _base + _side1 + _side2;
}

@end

// Main function
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an instance of each shape
        Circle *circle = [[Circle alloc] initWithRadius:5.0];
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10.0 height:5.0];
        Triangle *triangle = [[Triangle alloc] initWithBase:10.0 height:8.0 side1:6.0 side2:8.0];

        // Print the area and perimeter of each shape
        NSLog(@"Circle area: %.2f", [circle area]);
        NSLog(@"Circle perimeter: %.2f", [circle perimeter]);
        NSLog(@"Rectangle area: %.2f", [rectangle area]);
        NSLog(@"Rectangle perimeter: %.2f", [rectangle perimeter]);
        NSLog(@"Triangle area: %.2f", [triangle area]);
        NSLog(@"Triangle perimeter: %.2f", [triangle perimeter]);
    }
    return 0;
}
```

Explanation:

1. Import the necessary header files:

```
#import <Foundation/Foundation.h>
```

2. Define a macro for convenience:

```
#define PI 3.14159265
```

This macro is used to represent the value of π.

3. Declare a protocol for shapes:

```
@protocol Shape
- (double)area;
- (double)perimeter;
@end
```

This protocol defines two methods that all shapes must implement: `area` and `perimeter`.

4. Implement the Shape protocol for a circle:

```
@interface Circle : NSObject <Shape>
{
    double _radius;
}

- (instancetype)initWithRadius:(double)radius;
- (double)area;
- (double)perimeter;
@end
```

The `Circle` class is a concrete implementation of the `Shape` protocol. It has a private instance variable `_radius` to store the radius of the circle.

5. Implement the Circle class:

```
@implementation Circle

- (instancetype)initWithRadius:(double)radius
{
    self = [super init];
    if (self) {
        _radius = radius;
    }
    return self;
}

- (double)area
{
    return PI * _radius * _radius;
}

- (double)perimeter
{
    return 2 * PI * _radius;
}

@end
```

The `Circle` class provides implementations for the `area` and `perimeter` methods.

6. Implement the Shape protocol for a rectangle:

```
@interface Rectangle : NSObject <Shape>
{
    double _width;
    double _height;
}

- (instancetype)initWithWidth:(double)width height:(double)height;
- (double)area;
- (double)perimeter;
@end
```

The `Rectangle` class is another concrete implementation of the `Shape` protocol. It has two private instance variables `_width` and `_height` to store the width and height of the rectangle.

7. Implement the Rectangle class:

```
@implementation Rectangle

- (instancetype)initWithWidth:(double)width height:(double)height
{
    self = [super init];
    if (self) {
        _width = width;
        _height = height;
    }
    return self;
}

- (double)area
{
    return _width * _height;
}

- (double)perimeter
{
    return 2 * (_width + _height);
}

@end
```

The `Rectangle` class provides implementations for the `area` and `perimeter` methods.

8. Implement the Shape protocol for a triangle:

```
@interface Triangle : NSObject <Shape>
{
    double _base;
    double _height;
    double _side1;
    double _side2;
}

- (instancetype)initWithBase:(double)base height:(double)height side1:(double)side1 side2:(double)side2;
- (double)area;
- (double)perimeter;
@end
```

The `Triangle` class is a third concrete implementation of the `Shape` protocol. It has four private instance variables `_base`, `_height`, `_side1`, and `_side2` to store the base, height, and two sides of the triangle.

9. Implement the Triangle class:

```
@implementation Triangle

- (instancetype)initWithBase:(double)base height:(double)height side1:(double)side1 side2:(double)side2
{
    self = [super init];
    if (self) {
        _base = base;
        _height = height;
        _side1 = side1;
        _side2 = side2;
    }
    return self;
}

- (double)area
{
    return 0.5 * _base * _height;
}

- (double)perimeter
{
    return _base + _side1 + _side2;
}

@end
```

The `Triangle` class provides implementations for the `area` and `perimeter` methods.

10. Main function:

```
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Create an instance of each shape
        Circle *circle = [[Circle alloc] initWithRadius:5.0];
        Rectangle *rectangle = [[Rectangle alloc] initWithWidth:10.0 height:5.0];
        Triangle *triangle = [[Triangle alloc] initWithBase:10.0 height:8.0 side1:6.0 side2:8.0];

        // Print the area and perimeter of each shape
        NSLog(@"Circle area: %.2f", [circle area]);
        NSLog(@"Circle perimeter: %.2f", [circle perimeter]);
        NSLog(@"Rectangle area: %.2f", [rectangle area]);
        NSLog(@"Rectangle perimeter: %.2f", [rectangle perimeter]);
        NSLog(@"Triangle area: %.2f", [triangle area]);
        NSLog(@"Triangle perimeter: %.2f", [triangle perimeter]);
    }
    return 0;
}