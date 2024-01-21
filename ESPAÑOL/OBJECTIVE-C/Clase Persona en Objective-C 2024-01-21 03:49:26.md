```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

- (id)initWithNombre:(NSString *)nombre edad:(int)edad;
- (NSString *)nombre;
- (int)edad;
- (void)setNombre:(NSString *)nombre;
- (void)setEdad:(int)edad;

@end

@implementation Persona

- (id)initWithNombre:(NSString *)nombre edad:(int)edad {
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

- (int)edad {
    return edad;
}

- (void)setNombre:(NSString *)nombre {
    self->nombre = nombre;
}

- (void)setEdad:(int)edad {
    self->edad = edad;
}

@end

int main(int argc, const char *argv[]) {
    @autoreleasepool {
        Persona *persona = [[Persona alloc] initWithNombre:@"Juan" edad:20];
        NSLog(@"Nombre: %@, Edad: %d", persona.nombre, persona.edad);
    }
    return 0;
}
```

Explicación del código:

* Se define una clase `Persona` que contiene dos propiedades, `nombre` y `edad`, y dos métodos para acceder y modificar esas propiedades.
* Se crea un objeto `Persona` con el nombre "Juan" y la edad 20.
* Se imprime el nombre y la edad de la persona en la consola.

Este código es un ejemplo sencillo de cómo usar la programación orientada a objetos en Objective-C.