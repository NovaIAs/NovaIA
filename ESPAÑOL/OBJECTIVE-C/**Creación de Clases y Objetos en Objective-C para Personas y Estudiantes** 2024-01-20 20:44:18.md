```objective-c
#import <Foundation/Foundation.h>

// Definición de la interfaz de la clase Persona
@interface Persona : NSObject {
    NSString *nombre;
    int edad;
}

// Propiedades de la clase Persona
@property (nonatomic, strong) NSString *nombre;
@property (nonatomic) int edad;

// Métodos de la clase Persona
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;
- (NSString *)saludo;

@end

// Implementación de la clase Persona
@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

- (NSString *)saludo {
    return [NSString stringWithFormat:@"Hola, mi nombre es %@ y tengo %d años.", self.nombre, self.edad];
}

@end

// Definición de la interfaz de la clase Estudiante
@interface Estudiante : Persona {
    NSString *universidad;
    NSString *carrera;
}

// Propiedades de la clase Estudiante
@property (nonatomic, strong) NSString *universidad;
@property (nonatomic, strong) NSString *carrera;

// Métodos de la clase Estudiante
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad carrera:(NSString *)carrera;
- (NSString *)saludo;

@end

// Implementación de la clase Estudiante
@implementation Estudiante

- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad universidad:(NSString *)universidad carrera:(NSString *)carrera {
    self = [super initWithNombre:nombre edad:edad];
    if (self) {
        self.universidad = universidad;
        self.carrera = carrera;
    }
    return self;
}

- (NSString *)saludo {
    return [NSString stringWithFormat:@"Hola, mi nombre es %@, tengo %d años, estudio en la universidad %@ y mi carrera es %@", self.nombre, self.edad, self.universidad, self.carrera];
}

@end

// Creación de objetos de tipo Persona y Estudiante
Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20];
Estudiante *estudiante1 = [[Estudiante alloc] initWithNombre:@"María" edad:22 universidad:@"Universidad de Chile" carrera:@"Ingeniería Civil"];

// Impresión del saludo de los objetos
NSLog(@"%@", persona1.saludo);
NSLog(@"%@", estudiante1.saludo);
```

Explicación del código:

* Se definen las clases Persona y Estudiante, que son clases en Objective-C.
* La clase Persona tiene dos propiedades, nombre y edad, y un método, saludo, que devuelve un saludo con el nombre y la edad de la persona.
* La clase Estudiante hereda de la clase Persona y añade dos propiedades más, universidad y carrera, y un método, saludo, que devuelve un saludo con el nombre, la edad, la universidad y la carrera del estudiante.
* Se crean objetos de tipo Persona y Estudiante, y se llama al método saludo para imprimir el saludo de cada objeto.

Este código es complejo porque:

* Utiliza herencia, que es un concepto avanzado de programación orientada a objetos.
* Define múltiples clases con diferentes propiedades y métodos.
* Utiliza el método saludo para imprimir el saludo de los objetos, que es una forma de polimorfismo.

En general, este código es un ejemplo de cómo utilizar las características avanzadas de Objective-C para crear código complejo y flexible.