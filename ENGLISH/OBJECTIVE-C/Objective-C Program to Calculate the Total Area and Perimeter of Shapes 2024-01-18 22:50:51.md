```objective-c
#import <Foundation/Foundation.h>

// Define a protocol for describing a geometric shape
@protocol Shape
- (double)area;
- (double)perimeter;
@end

// Define a class for a circle
@interface Circle : NSObject <Shape>
- (instancetype)initWithRadius:(double)radius;
@property (readonly) double radius;
@end

@implementation Circle
- (instancetype)initWithRadius:(double)radius {
    self = [super init];
    if (self) {
        _radius = radius;
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

// Define a class for a rectangle
@interface Rectangle : NSObject <Shape>
- (instancetype)initWithWidth:(double)width height:(double)height;
@property (readonly) double width;
@property (readonly) double height;
@end

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
    return self.width * self.height;
}

- (double)perimeter {
    return 2 * (self.width + self.height);
}
@end

// Define a class for a triangle
@interface Triangle : NSObject <Shape>
- (instancetype)initWithBase:(double)base height:(double)height;
@property (readonly) double base;
@property (readonly) double height;
@end

@implementation Triangle
- (instancetype)initWithBase:(double)base height:(double)height {
    self = [super init];
    if (self) {
        _base = base;
        _height = height;
    }
    return self;
}

- (double)area {
    return 0.5 * self.base * self.height;
}

- (double)perimeter {
    double side1 = sqrt(self.base * self.base + self.height * self.height);
    double side2 = sqrt(self.base * self.base + self.height * self.height);
    return self.base + side1 + side2;
}
@end

// Define a function to calculate the total area of an array of shapes
double totalArea(NSArray<id<Shape>> *shapes) {
    double total = 0;
    for (id<Shape> shape in shapes) {
        total += [shape area];
    }
    return total;
}

// Define a function to calculate the total perimeter of an array of shapes
double totalPerimeter(NSArray<id<Shape>> *shapes) {
    double total =