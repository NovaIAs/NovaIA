```objective-c
// Importamos las bibliotecas necesarias
#import <Foundation/Foundation.h>

// Creamos una clase llamada "CalculadoraCompleja"
@interface CalculadoraCompleja : NSObject

// Definimos las propiedades de la clase
@property (nonatomic) double real;
@property (nonatomic) double imaginaria;

// Definimos los métodos de la clase
- (id)initWithReal:(double)real imaginaria:(double)imaginaria;
- (CalculadoraCompleja *)suma:(CalculadoraCompleja *)complejo;
- (CalculadoraCompleja *)resta:(CalculadoraCompleja *)complejo;
- (CalculadoraCompleja *)multiplicacion:(CalculadoraCompleja *)complejo;
- (CalculadoraCompleja *)division:(CalculadoraCompleja *)complejo;
- (NSString *)descripcion;

@end

// Implementamos los métodos de la clase
@implementation CalculadoraCompleja

// Método inicializador
- (id)initWithReal:(double)real imaginaria:(double)imaginaria {
    self = [super init];
    if (self) {
        self.real = real;
        self.imaginaria = imaginaria;
    }
    return self;
}

// Método para sumar dos números complejos
- (CalculadoraCompleja *)suma:(CalculadoraCompleja *)complejo {
    CalculadoraCompleja *resultado = [[CalculadoraCompleja alloc] init];
    resultado.real = self.real + complejo.real;
    resultado.imaginaria = self.imaginaria + complejo.imaginaria;
    return resultado;
}

// Método para restar dos números complejos
- (CalculadoraCompleja *)resta:(CalculadoraCompleja *)complejo {
    CalculadoraCompleja *resultado = [[CalculadoraCompleja alloc] init];
    resultado.real = self.real - complejo.real;
    resultado.imaginaria = self.imaginaria - complejo.imaginaria;
    return resultado;
}

// Método para multiplicar dos números complejos
- (CalculadoraCompleja *)multiplicacion:(CalculadoraCompleja *)complejo {
    CalculadoraCompleja *resultado = [[CalculadoraCompleja alloc] init];
    resultado.real = self.real * complejo.real - self.imaginaria * complejo.imaginaria;
    resultado.imaginaria = self.real * complejo.imaginaria + self.imaginaria * complejo.real;
    return resultado;
}

// Método para dividir dos números complejos
- (CalculadoraCompleja *)division:(CalculadoraCompleja *)complejo {
    CalculadoraCompleja *resultado = [[CalculadoraCompleja alloc] init];
    double denominador = complejo.real * complejo.real + complejo.imaginaria * complejo.imaginaria;
    resultado.real = (self.real * complejo.real + self.imaginaria * complejo.imaginaria) / denominador;
    resultado.imaginaria = (self.imaginaria * complejo.real - self.real * complejo.imaginaria) / denominador;
    return resultado;
}

// Método para obtener una descripción del número complejo
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"(%f, %fi)", self.real, self.imaginaria];
}

@end

// Creamos una función principal para probar la clase
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos dos números complejos
        CalculadoraCompleja *complejo1 = [[CalculadoraCompleja alloc] initWithReal:3 imaginaria:4];
        CalculadoraCompleja *complejo2 = [[CalculadoraCompleja alloc] initWithReal:5 imaginaria:-2];

        // Realizamos las operaciones básicas
        CalculadoraCompleja *suma = [complejo1 suma:complejo2];
        CalculadoraCompleja *resta = [complejo1 resta:complejo2];
        CalculadoraCompleja *multiplicacion = [complejo1 multiplicacion:complejo2];
        CalculadoraCompleja *division = [complejo1 division:complejo2];

        // Imprimimos los resultados
        NSLog(@"Suma: %@", suma);
        NSLog(@"Resta: %@", resta);
        NSLog(@"Multiplicación: %@", multiplicacion);
        NSLog(@"División: %@", division);
    }
    return 0;
}
```

Explicación del código:

* La clase `CalculadoraCompleja` representa números complejos con propiedades `real` e `imaginaria`.
* Los métodos `suma`, `resta`, `multiplicacion` y `division` implementan las operaciones básicas para números complejos.
* El método `descripcion` devuelve una representación en cadena de un número complejo.
* La función principal `main` crea dos números complejos, realiza las operaciones básicas y muestra los resultados.