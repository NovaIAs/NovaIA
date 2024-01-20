```objective-c
#import <Foundation/Foundation.h>

// Clase Persona
@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

// Propiedades
@property (nonatomic, retain) NSString *nombre;
@property (nonatomic, assign) int edad;

// Métodos
- (id)initWithNombre:(NSString *)nombre edad:(int)edad;
- (NSString *)descripcion;

@end

// Implementación de la clase Persona
@implementation Persona

// Constructor
- (id)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Descripción
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Nombre: %@, Edad: %d", self.nombre, self.edad];
}

@end

// Clase Estudiante
@interface Estudiante : Persona {
    double promedio;
}

// Propiedades
@property (nonatomic, assign) double promedio;

// Métodos
- (id)initWithNombre:(NSString *)nombre edad:(int)edad promedio:(double)promedio;
- (NSString *)descripcion;

@end

// Implementación de la clase Estudiante
@implementation Estudiante

// Constructor
- (id)initWithNombre:(NSString *)nombre edad:(int)edad promedio:(double)promedio {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        self.promedio = promedio;
    }
    return self;
}

// Descripción
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"%@, Promedio: %.2f", [super descripcion], self.promedio];
}

@end

// Clase Profesor
@interface Profesor : Persona {
    NSString *materia;
}

// Propiedades
@property (nonatomic, retain) NSString *materia;

// Métodos
- (id)initWithNombre:(NSString *)nombre edad:(int)edad materia:(NSString *)materia;
- (NSString *)descripcion;

@end

// Implementación de la clase Profesor
@implementation Profesor

// Constructor
- (id)initWithNombre:(NSString *)nombre edad:(int)edad materia:(NSString *)materia {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        self.materia = materia;
    }
    return self;
}

// Descripción
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"%@, Materia: %@", [super descripcion], self.materia];
}

@end

// Función principal
int main(int argc, char *argv[]) {
    @autoreleasepool {
        // Crear objetos
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20];
        Estudiante *estudiante1 = [[Estudiante alloc] initWithNombre:@"María" edad:18 promedio:3.8];
        Profesor *profesor1 = [[Profesor alloc] initWithNombre:@"Pedro" edad:35 materia:@"Matemáticas"];

        // Imprimir descripciones
        NSLog(@"%@", [persona1 descripcion]);
        NSLog(@"%@", [estudiante1 descripcion]);
        NSLog(@"%@", [profesor1 descripcion]);

        // Liberar objetos
        [persona1 release];
        [estudiante1 release];
        [profesor1 release];
    }
    return 0;
}
```

**Explicación del código:**

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` tiene dos propiedades: `nombre` y `edad`.
* La clase `Estudiante` hereda de la clase `Persona` y añade una propiedad `promedio`.
* La clase `Profesor` hereda de la clase `Persona` y añade una propiedad `materia`.
* Se definen funciones constructoras para cada clase.
* Se definen funciones `descripcion` para cada clase, que devuelven una cadena con la información del objeto.
* En la función `main`, se crean objetos de cada clase y se imprimen sus descripciones.
* Finalmente, se liberan los objetos creados.

Este código es un ejemplo de uso de herencia y polimorfismo en Objective-C.