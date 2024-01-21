```objective-c
// Importación de bibliotecas necesarias
#import <Foundation/Foundation.h>

// Definición de una clase en Objective-C
@interface Calculadora : NSObject

// Método para inicializar la calculadora
- (instancetype)init;

// Métodos para realizar operaciones matemáticas
- (double)suma:(double)operando1 con:(double)operando2;
- (double)resta:(double)operando1 con:(double)operando2;
- (double)multiplicacion:(double)operando1 con:(double)operando2;
- (double)division:(double)operando1 con:(double)operando2;

@end

// Implementación de los métodos de la clase Calculadora
@implementation Calculadora

- (instancetype)init {
    self = [super init];
    if (self) {
        // Inicialización del objeto
    }
    return self;
}

- (double)suma:(double)operando1 con:(double)operando2 {
    return operando1 + operando2;
}

- (double)resta:(double)operando1 con:(double)operando2 {
    return operando1 - operando2;
}

- (double)multiplicacion:(double)operando1 con:(double)operando2 {
    return operando1 * operando2;
}

- (double)division:(double)operando1 con:(double)operando2 {
    if (operando2 == 0) {
        @throw [NSException exceptionWithName:@"DivisionPorCero" reason:@"No se puede dividir por cero" userInfo:nil];
    }
    return operando1 / operando2;
}

@end

// Función principal del programa
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creación de una instancia de la clase Calculadora
        Calculadora *calculadora = [[Calculadora alloc] init];

        // Uso de los métodos de la calculadora para realizar operaciones matemáticas
        double resultadoSuma = [calculadora suma:5 con:10];
        double resultadoResta = [calculadora resta:15 con:7];
        double resultadoMultiplicacion = [calculadora multiplicacion:3 con:4];
        double resultadoDivision = [calculadora division:20 con:5];

        // Impresión de los resultados en la consola
        NSLog(@"Resultado de la suma: %f", resultadoSuma);
        NSLog(@"Resultado de la resta: %f", resultadoResta);
        NSLog(@"Resultado de la multiplicación: %f", resultadoMultiplicacion);
        NSLog(@"Resultado de la división: %f", resultadoDivision);
    }
    return 0;
}
```

El código implementa una calculadora simple en Objective-C. La clase `Calculadora` define los métodos para realizar operaciones matemáticas básicas (suma, resta, multiplicación y división). La función `main` crea una instancia de la clase `Calculadora` y la utiliza para realizar algunas operaciones matemáticas. Los resultados se imprimen en la consola.

El código está bien diseñado y es fácil de entender. Utiliza buenas prácticas de programación, como el uso de objetos y la gestión de excepciones. Es un buen ejemplo de cómo crear una aplicación simple en Objective-C.