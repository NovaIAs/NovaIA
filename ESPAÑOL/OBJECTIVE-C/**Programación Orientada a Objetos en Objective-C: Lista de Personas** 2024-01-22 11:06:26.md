```objective-c
#import <Foundation/Foundation.h>

// Clase Persona
@interface Persona : NSObject {
    NSString *_nombre;
    int _edad;
}

// Propiedades
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, assign) int edad;

// Métodos
- (id)initWithNombre:(NSString *)nombre edad:(int)edad;
- (NSString *)descripcion;

@end

// Implementación de la clase Persona
@implementation Persona

// Método inicializador
- (id)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _edad = edad;
    }
    return self;
}

// Método que devuelve una descripción de la persona
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Nombre: %@, Edad: %d", _nombre, _edad];
}

@end

// Clase ListaPersonas
@interface ListaPersonas : NSObject {
    NSMutableArray *_personas;
}

// Propiedades
@property (nonatomic, strong) NSMutableArray *personas;

// Métodos
- (id)init;
- (void)agregarPersona:(Persona *)persona;
- (NSString *)descripcion;

@end

// Implementación de la clase ListaPersonas
@implementation ListaPersonas

// Método inicializador
- (id)init {
    self = [super init];
    if (self) {
        _personas = [[NSMutableArray alloc] init];
    }
    return self;
}

// Método que agrega una persona a la lista
- (void)agregarPersona:(Persona *)persona {
    [_personas addObject:persona];
}

// Método que devuelve una descripción de la lista de personas
- (NSString *)descripcion {
    NSMutableString *descripcion = [[NSMutableString alloc] init];
    for (Persona *persona in _personas) {
        [descripcion appendString:[persona descripcion]];
        [descripcion appendString:@"\n"];
    }
    return descripcion;
}

@end

// Función principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una lista de personas
        ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];

        // Agregar algunas personas a la lista
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Juan" edad:20]];
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"María" edad:25]];
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Pedro" edad:30]];

        // Imprimir la lista de personas
        NSLog(@"%@", [listaPersonas descripcion]);
    }
    return 0;
}
```

Explicación del código:

* La clase `Persona` representa una persona con un nombre y una edad.
* La clase `ListaPersonas` representa una lista de personas.
* La función `main()` es el punto de entrada del programa.
* En la función `main()`, se crea una lista de personas y se agregan algunas personas a la lista.
* Finalmente, se imprime la lista de personas en la consola.

Este código es un ejemplo de cómo se puede usar la programación orientada a objetos en Objective-C. El código está organizado en diferentes clases, cada una de las cuales representa un concepto diferente. Esto hace que el código sea más fácil de leer y mantener.