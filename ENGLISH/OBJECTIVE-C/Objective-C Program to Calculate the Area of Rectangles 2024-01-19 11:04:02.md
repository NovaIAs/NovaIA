```objective-c
#import <Foundation/Foundation.h>

typedef struct {
    int width;
    int height;
} Rectangle;

Rectangle createRectangle(int width, int height) {
    Rectangle rectangle;
    rectangle.width = width;
    rectangle.height = height;
    return rectangle;
}

int calculateRectangleArea(Rectangle rectangle) {
    return rectangle.width * rectangle.height;
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Rectangle rectangle1 = createRectangle(10, 5);
        int area1 = calculateRectangleArea(rectangle1);
        
        Rectangle rectangle2 = createRectangle(20, 10);
        int area2 = calculateRectangleArea(rectangle2);
        
        NSLog(@"Rectangle 1 dimensions: %d x %d, area: %d", rectangle1.width, rectangle1.height, area1);
        NSLog(@"Rectangle 2 dimensions: %d x %d, area: %d", rectangle2.width, rectangle2.height, area2);
    }
    return 0;
}
```

Explanation:

1. We define a `Rectangle` struct to represent a rectangle with `width` and `height` properties.

2. We create a function `createRectangle` that takes two integer arguments (`width` and `height`) and returns a `Rectangle` struct with those values.

3. We create a function `calculateRectangleArea` that takes a `Rectangle` as an argument and returns the area of the rectangle by multiplying the `width` and `height` properties.

4. In the `main` function, we create two `Rectangle` objects using the `createRectangle` function and calculate their areas using the `calculateRectangleArea` function.

5. We use `NSLog` to print the dimensions and areas of the rectangles to the console.

This code demonstrates the use of structs, functions, and basic arithmetic operations in Objective-C.