```objective-c
// Definición de una clase "Sumador" con dos métodos de instancia
@interface Sumador : NSObject
-(int)suma:(int)num1 con:(int)num2;
-(void)imprimirSuma:(int)num1 con:(int)num2;
@end

// Implementación de la clase Sumador
@implementation Sumador
-(int)suma:(int)num1 con:(int)num2 {
    return num1 + num2;
}

-(void)imprimirSuma:(int)num1 con:(int)num2 {
    printf("%d + %d = %d\n", num1, num2, [self suma:num1 con:num2]);
}
@end

// Definición de una clase "Calculadora" que utiliza la clase "Sumador"
@interface Calculadora : NSObject
-(int)multiplicar:(int)num1 por:(int)num2;
-(void)imprimirProducto:(int)num1 por:(int)num2;

@property (nonatomic, strong) Sumador *sumador; // Propiedad para almacenar una instancia de la clase Sumador
@end

// Implementación de la clase Calculadora
@implementation Calculadora
-(int)multiplicar:(int)num1 por:(int)num2 {
    return num1 * num2;
}

-(void)imprimirProducto:(int)num1 por:(int)num2 {
    printf("%d x %d = %d\n", num1, num2, [self multiplicar:num1 por:num2]);
}

-(void)setSumador:(Sumador *)nuevoSumador {
    if (_sumador != nuevoSumador) {
        _sumador = nuevoSumador;
    }
}

-(Sumador *)sumador {
    if (_sumador == nil) {
        _sumador = [[Sumador alloc] init];
    }
    return _sumador;
}
@end

// Código principal para probar las clases "Sumador" y "Calculadora"
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Sumador *sumador = [[Sumador alloc] init];
        [sumador imprimirSuma:5 con:10];
        
        Calculadora *calculadora = [[Calculadora alloc] init];
        [calculadora imprimirProducto:7 por:8];
        
        [calculadora.sumador imprimirSuma:3 con:4];
    }
    return 0;
}
```

**Explicación del código:**

* La clase `Sumador` tiene dos métodos de instancia: `suma:` y `imprimirSuma:`. El método `suma:` devuelve la suma de dos números, mientras que el método `imprimirSuma:` imprime la suma de dos números en la consola.
* La clase `Calculadora` tiene dos métodos de instancia: `multiplicar:` e `imprimirProducto:` El método `multiplicar:` devuelve el producto de dos números, mientras que el método `imprimirProducto:` imprime el producto de dos números en la consola.
* La clase `Calculadora` también tiene una propiedad `sumador` de tipo `Sumador`. Esta propiedad se utiliza para almacenar una instancia de la clase `Sumador`.
* El método `setSumador:` se utiliza para establecer el valor de la propiedad `sumador`.
* El método `sumador` devuelve la instancia de la clase `Sumador` almacenada en la propiedad `sumador`. Si la propiedad `sumador` es nula, el método `sumador` crea una nueva instancia de la clase `Sumador`.
* En el código principal, se crea una instancia de la clase `Sumador` y se utiliza para imprimir la suma de dos números en la consola.
* También se crea una instancia de la clase `Calculadora` y se utiliza para imprimir el producto de dos números en la consola.
* Por último, se utiliza la propiedad `sumador` de la instancia de la clase `Calculadora` para imprimir la suma de dos números en la consola.