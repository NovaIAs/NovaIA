```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
}

// Constructor
- (instancetype)initWithNombre:(NSString *)nombre andEdad:(int)edad;

// Métodos de acceso
- (NSString *)nombre;
- (void)setNombre:(NSString *)nombre;
- (int)edad;
- (void)setEdad:(int)edad;

// Métodos adicionales
- (NSString *)saludar;
- (void)crescer;
@end

// Definición de la interfaz de la clase Persona

@implementation Persona

// Implementación del constructor
- (instancetype)initWithNombre:(NSString *)nombre andEdad:(int)edad {
    self = [super init];
    self.nombre = nombre;
    self.edad = edad;
    return self;
}

// Implementación de los métodos de acceso
- (NSString *)nombre {
    return nombre;
}

- (void)setNombre:(NSString *)nombre {
    nombre = nombre;
}

- (int)edad {
    return edad;
}

- (void)setEdad:(int)edad {
    edad = edad;
}

// Implementación de los métodos adicionales
- (NSString *)saludar {
    return [NSString stringWithFormat:@"Hola, me llamo %@ y tengo %d años", nombre, edad];
}

- (void)crescer {
    edad++;
}
@end

// Definición de la implementación de la clase Persona

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos una instancia de la clase Persona
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" andEdad:20];
        
        // Imprimimos el saludo de la persona
        NSLog(@"%@", [persona saludar]);
        
        // Hacemos que la persona crezca
        [persona crecer];
        
        // Imprimimos el saludo de la persona de nuevo
        NSLog(@"%@", [persona saludar]);
    }
    return 0;
}
```

Explicación del código:

* La clase `Persona` define una estructura de datos para representar a una persona con su nombre y edad.
* El constructor `initWithNombre:andEdad:` inicializa una instancia de la clase `Persona` con los parámetros proporcionados.
* Los métodos de acceso `nombre`, `setNombre`, `edad` y `setEdad` permiten acceder y modificar los atributos `nombre` y `edad` de la persona.
* El método `saludar` devuelve un saludo de la persona.
* El método `crescer` incrementa la edad de la persona.
* En la función `main` se crea una instancia de la clase `Persona` con el nombre "Juan" y la edad 20.
* Se imprime el saludo de la persona.
* Se hace que la persona crezca.
* Se imprime el saludo de la persona de nuevo.