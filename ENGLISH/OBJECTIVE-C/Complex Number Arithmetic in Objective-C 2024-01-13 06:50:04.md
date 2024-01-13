```objective-c
#import <Foundation/Foundation.h>

// Define a macro to make code more readable
#define PI 3.14159265358979323846

// Define a custom exception class
@interface MathException : NSException
@end

// Implementation of the custom exception class
@implementation MathException
@end

// Define a class to represent a complex number
@interface ComplexNumber : NSObject
{
    double real;    // The real part of the complex number
    double imaginary; // The imaginary part of the complex number
}

// Initialize a complex number with the given real and imaginary parts
- (instancetype)initWithReal:(double)real imaginary:(double)imaginary;

// Add two complex numbers together
- (ComplexNumber *)add:(ComplexNumber *)other;

// Subtract one complex number from another
- (ComplexNumber *)subtract:(ComplexNumber *)other;

// Multiply two complex numbers together
- (ComplexNumber *)multiply:(ComplexNumber *)other;

// Divide one complex number by another
- (ComplexNumber *)divide:(ComplexNumber *)other;

// Calculate the absolute value of a complex number
- (double)absoluteValue;

// Calculate the argument of a complex number
- (double)argument;

// Return a string representation of the complex number
- (NSString *)description;
@end

// Implementation of the ComplexNumber class
@implementation ComplexNumber

// Initialize a complex number with the given real and imaginary parts
- (instancetype)initWithReal:(double)real imaginary:(double)imaginary
{
    self = [super init];
    if (self) {
        self.real = real;
        self.imaginary = imaginary;
    }
    return self;
}

// Add two complex numbers together
- (ComplexNumber *)add:(ComplexNumber *)other
{
    ComplexNumber *result = [[ComplexNumber alloc] init];
    result.real = self.real + other.real;
    result.imaginary = self.imaginary + other.imaginary;
    return result;
}

// Subtract one complex number from another
- (ComplexNumber *)subtract:(ComplexNumber *)other
{
    ComplexNumber *result = [[ComplexNumber alloc] init];
    result.real = self.real - other.real;
    result.imaginary = self.imaginary - other.imaginary;
    return result;
}

// Multiply two complex numbers together
- (ComplexNumber *)multiply:(ComplexNumber *)other
{
    ComplexNumber *result = [[ComplexNumber alloc] init];
    result.real = self.real * other.real - self.imaginary * other.imaginary;
    result.imaginary = self.real * other.imaginary + self.imaginary * other.real;
    return result;
}

// Divide one complex number by another
- (ComplexNumber *)divide:(ComplexNumber *)other
{
    // Check for division by zero
    if (other.real == 0 && other.imaginary == 0) {
        @throw [MathException exceptionWithName:@"DivisionByZero" reason:@"Cannot divide by zero" userInfo:nil];
    }

    // Calculate the denominator
    double denominator = other.real * other.real + other.imaginary * other.imaginary;

    // Calculate the result
    ComplexNumber *result = [[ComplexNumber alloc] init];
    result.real = (self.real * other.real + self.imaginary * other.imaginary) / denominator;
    result.imaginary = (self.imaginary * other.real - self.real * other.imaginary) / denominator;
    return result;
}

// Calculate the absolute value of a complex number
- (double)absoluteValue
{
    return sqrt(self.real * self.real + self.imaginary * self.imaginary);
}

// Calculate the argument of a complex number
- (double)argument
{
    return atan2(self.imaginary, self.real);
}

// Return a string representation of the complex number
- (NSString *)description
{
    return [NSString stringWithFormat:@"(%f, %f)", self.real, self.imaginary];
}

@end

// Main function
int main(int argc, const char * argv[])
{
    @autoreleasepool {
        // Create two complex numbers
        ComplexNumber *a = [[ComplexNumber alloc] initWithReal:3 imaginary:4];
        ComplexNumber *b = [[ComplexNumber alloc] initWithReal:5 imaginary:-2];

        // Add the two complex numbers together
        ComplexNumber *sum = [a add:b];

        // Subtract one complex number from another
        ComplexNumber *difference = [a subtract:b];

        // Multiply the two complex numbers together
        ComplexNumber *product = [a multiply:b];

        // Divide one complex number by another
        ComplexNumber *quotient = [a divide:b];

        // Calculate the absolute value of a complex number