```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

- (id)initWithNombre:(NSString *)nombre Edad:(int)edad;
- (NSString *)nombre;
- (void)setNombre:(NSString *)nombre;
- (int)edad;
- (void)setEdad:(int)edad;

@end

@implementation Persona

- (id)initWithNombre:(NSString *)nombre Edad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

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

@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" Edad:20];

        NSLog(@"Nombre: %@", persona.nombre);
        NSLog(@"Edad: %d", persona.edad);

        persona.nombre = @"María";
        persona.edad = 25;

        NSLog(@"Nombre: %@", persona.nombre);
        NSLog(@"Edad: %d", persona.edad);
    }
    return 0;
}
```

Explicación del código:

* Se define una clase `Persona` que representa a una persona con un nombre y una edad.
* La clase `Persona` tiene dos propiedades: `nombre` y `edad`.
* La propiedad `nombre` es de tipo `NSString` y representa el nombre de la persona.
* La propiedad `edad` es de tipo `int` y representa la edad de la persona.
* Se definen los métodos `initWithNombre:Edad:` y `setNombre:`, `setEdad:` para inicializar y modificar las propiedades `nombre` y `edad`.
* Se crea una instancia de la clase `Persona` con el nombre "Juan" y la edad 20.
* Se imprime el nombre y la edad de la persona.
* Se modifica el nombre y la edad de la persona.
* Se imprime el nombre y la edad de la persona nuevamente.