```objective-c
// Importamos las bibliotecas necesarias

#import <Foundation/Foundation.h>

// Creamos una clase llamada "Ejemplo"
@interface Ejemplo : NSObject

// Declaramos las propiedades de la clase
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSNumber *edad;

// Creamos un método para inicializar la clase
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSNumber *)edad;

// Creamos un método para imprimir el nombre y la edad de una persona
- (void)imprimirNombreYEdad;

@end

// Implementamos la clase "Ejemplo"
@implementation Ejemplo

// Implementamos el método para inicializar la clase
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSNumber *)edad {
  if (self = [super init]) {
    _nombre = nombre;
    _edad = edad;
  }
  return self;
}

// Implementamos el método para imprimir el nombre y la edad de una persona
- (void)imprimirNombreYEdad {
  NSLog(@"Nombre: %@, Edad: %@", self.nombre, self.edad);
}

@end

// Creamos una función principal
int main(int argc, const char * argv[]) {
  @autoreleasepool {
      
    // Creamos una instancia de la clase "Ejemplo"
    Ejemplo *ejemplo = [[Ejemplo alloc] initWithNombre:@"Juan" edad:@20];
    
    // Imprimimos el nombre y la edad de la persona
    [ejemplo imprimirNombreYEdad];
    
  }
  return 0;
}
```

Explicación del código:

1. Importamos la biblioteca `Foundation` que contiene las clases y métodos básicos del lenguaje Objective-C.


2. Creamos una clase llamada `Ejemplo` que es una subclase de `NSObject`. Esta clase representa una "persona" y tiene dos propiedades: `nombre` y `edad`.


3. Implementamos el método `- (instancetype)initWithNombre:(NSString *)nombre edad:(NSNumber*)edad` que es el inicializador de la clase. Este método recibe dos parámetros: `nombre` y `edad`, y los asigna a las propiedades correspondientes.


4. Implementamos el método `- (void)imprimirNombreYEdad` que imprime el nombre y la edad de la persona en la consola.


5. En la función `main` creamos una instancia de la clase `Ejemplo` pasándole los valores "Juan" y 20 a las propiedades `nombre` y `edad` respectivamente.


6. Llamamos al método `imprimirNombreYEdad` para imprimir el nombre y la edad de la persona en la consola.

Cuando ejecutamos el código, se imprimirá en la consola:

```
Nombre: Juan, Edad: 20
```