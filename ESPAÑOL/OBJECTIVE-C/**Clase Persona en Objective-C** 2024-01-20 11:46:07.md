```objective-c
#import <Foundation/Foundation.h>

// Definición de la clase Persona
@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

// Propiedades de la clase Persona
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Método inicializador de la clase Persona
- (instancetype)initWithNombre:(NSString *)nombre andEdad:(int)edad;

// Método para obtener el nombre de la persona
- (NSString *)nombre;

// Método para obtener la edad de la persona
- (int)edad;

// Método para imprimir el nombre y la edad de la persona
- (void)imprimirNombreYEdad;

@end

// Implementación de la clase Persona
@implementation Persona

// Método inicializador de la clase Persona
- (instancetype)initWithNombre:(NSString *)nombre andEdad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Método para obtener el nombre de la persona
- (NSString *)nombre {
    return self.nombre;
}

// Método para obtener la edad de la persona
- (int)edad {
    return self.edad;
}

// Método para imprimir el nombre y la edad de la persona
- (void)imprimirNombreYEdad {
    NSLog(@"Nombre: %@, Edad: %d", self.nombre, self.edad);
}

@end

// Función principal del programa
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creación de una instancia de la clase Persona
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" andEdad:25];
        
        // Impresión del nombre y la edad de la persona
        [persona1 imprimirNombreYEdad];
    }
    return 0;
}
```

Explicación del código:

1. **Definición de la clase Persona**: Se define una clase llamada `Persona` que representa a una persona con un nombre y una edad.

2. **Propiedades de la clase Persona**: Se definen dos propiedades para la clase `Persona`: `nombre` y `edad`. La propiedad `nombre` es de tipo `NSString` y la propiedad `edad` es de tipo `int`.

3. **Método inicializador de la clase Persona**: Se define un método inicializador para la clase `Persona` que toma como parámetros el nombre y la edad de la persona. El método inicializador es responsable de crear una nueva instancia de la clase `Persona` y asignar los valores de los parámetros a las propiedades de la instancia.

4. **Métodos para obtener el nombre y la edad de la persona**: Se definen dos métodos para obtener el nombre y la edad de la persona: `nombre` y `edad`. Estos métodos retornan los valores de las propiedades correspondientes.

5. **Método para imprimir el nombre y la edad de la persona**: Se define un método llamado `imprimirNombreYEdad` que imprime el nombre y la edad de la persona en la consola.

6. **Función principal del programa**: Se define la función principal del programa, que es el punto de entrada del programa.

7. **Creación de una instancia de la clase Persona**: Se crea una instancia de la clase `Persona` llamada `persona1` y se le asigna un nombre y una edad.

8. **Impresión del nombre y la edad de la persona**: Se llama al método `imprimirNombreYEdad` en la instancia `persona1` para imprimir el nombre y la edad de la persona en la consola.