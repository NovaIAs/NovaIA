// MyComplexCode.h

// Clase ComplexNumber para representar números complejos.
#import <Foundation/Foundation.h>

@interface ComplexNumber : NSObject

// Propiedades para la parte real e imaginaria del número complejo.
@property (nonatomic) CGFloat real;
@property (nonatomic) CGFloat imaginary;

// Inicializador para crear un número complejo a partir de sus partes reales e imaginarias.
- (instancetype)initWithReal:(CGFloat)real imaginary:(CGFloat)imaginary;

// Métodos para sumar, restar, multiplicar y dividir números complejos.
- (ComplexNumber *)sum:(ComplexNumber *)otherNumber;
- (ComplexNumber *)subtract:(ComplexNumber *)otherNumber;
- (ComplexNumber *)multiply:(ComplexNumber *)otherNumber;
- (ComplexNumber *)divide:(ComplexNumber *)otherNumber;

// Método para obtener el módulo del número complejo.
- (CGFloat)modulus;

// Método para obtener el argumento del número complejo.
- (CGFloat)argument;

@end

// MyComplexCode.m

// Implementación de la clase ComplexNumber.
#import "MyComplexCode.h"

@implementation ComplexNumber

// Inicializador para crear un número complejo a partir de sus partes reales e imaginarias.
- (instancetype)initWithReal:(CGFloat)real imaginary:(CGFloat)imaginary {
    self = [super init];
    if (self) {
        _real = real;
        _imaginary = imaginary;
    }
    return self;
}

// Método para sumar dos números complejos.
- (ComplexNumber *)sum:(ComplexNumber *)otherNumber {
    CGFloat newReal = self.real + otherNumber.real;
    CGFloat newImaginary = self.imaginary + otherNumber.imaginary;
    return [[ComplexNumber alloc] initWithReal:newReal imaginary:newImaginary];
}

// Método para restar dos números complejos.
- (ComplexNumber *)subtract:(ComplexNumber *)otherNumber {
    CGFloat newReal = self.real - otherNumber.real;
    CGFloat newImaginary = self.imaginary - otherNumber.imaginary;
    return [[ComplexNumber alloc] initWithReal:newReal imaginary:newImaginary];
}

// Método para multiplicar dos números complejos.
- (ComplexNumber *)multiply:(ComplexNumber *)otherNumber {
    CGFloat newReal = self.real * otherNumber.real - self.imaginary * otherNumber.imaginary;
    CGFloat newImaginary = self.real * otherNumber.imaginary + self.imaginary * otherNumber.real;
    return [[ComplexNumber alloc] initWithReal:newReal imaginary:newImaginary];
}

// Método para dividir dos números complejos.
- (ComplexNumber *)divide:(ComplexNumber *)otherNumber {
    CGFloat denominator = otherNumber.real * otherNumber.real + otherNumber.imaginary * otherNumber.imaginary;
    CGFloat newReal = (self.real * otherNumber.real + self.imaginary * otherNumber.imaginary) / denominator;
    CGFloat newImaginary = (self.imaginary * otherNumber.real - self.real * otherNumber.imaginary) / denominator;
    return [[ComplexNumber alloc] initWithReal:newReal imaginary:newImaginary];
}

// Método para obtener el módulo del número complejo.
- (CGFloat)modulus {
    return sqrtf(self.real * self.real + self.imaginary * self.imaginary);
}

// Método para obtener el argumento del número complejo.
- (CGFloat)argument {
    return atan2f(self.imaginary, self.real);
}

@end

// Uso del código:

// Crear dos números complejos.
ComplexNumber *number1 = [[ComplexNumber alloc] initWithReal:1.0 imaginary:2.0];
ComplexNumber *number2 = [[ComplexNumber alloc] initWithReal:3.0 imaginary:4.0];

// Sumar los dos números complejos.
ComplexNumber *result1 = [number1 sum:number2];

// Multiplicar los dos números complejos.
ComplexNumber *result2 = [number1 multiply:number2];

// Imprimir los resultados.
NSLog(@"Resultado de la suma: (%f + %fi)", result1.real, result1.imaginary);
NSLog(@"Resultado de la multiplicación: (%f + %fi)", result2.real, result2.imaginary);