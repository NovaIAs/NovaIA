```objective-C

#import <Foundation/Foundation.h>

// Define una clase llamada "Persona" con propiedades para el nombre y la edad.
@interface Persona : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic) NSInteger edad;

// Inicializador designado que toma el nombre y la edad como parámetros.
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad;

// Método que devuelve una descripción de la persona.
- (NSString *)descripcion;

@end

// Implementación de la clase "Persona".
@implementation Persona

// Implementación del inicializador designado.
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Implementación del método que devuelve una descripción de la persona.
- (NSString *)descripcion {
    return [NSString stringWithFormat:@"Persona: %@, Edad: %ld", self.nombre, self.edad];
}

@end

// Define una clase llamada "ListaDePersonas" que es una colección de personas.
@interface ListaDePersonas : NSObject

@property (nonatomic, strong) NSMutableArray<Persona *> *personas;

// Inicializador designado que toma un array de personas como parámetro.
- (instancetype)initWithPersonas:(NSArray<Persona *> *)personas;

// Método que agrega una persona a la lista.
- (void)agregarPersona:(Persona *)persona;

// Método que elimina una persona de la lista.
- (void)eliminarPersona:(Persona *)persona;

// Método que devuelve una descripción de la lista de personas.
- (NSString *)descripcion;

@end

// Implementación de la clase "ListaDePersonas".
@implementation ListaDePersonas

// Implementación del inicializador designado.
- (instancetype)initWithPersonas:(NSArray<Persona *> *)personas {
    self = [super init];
    if (self) {
        self.personas = [personas mutableCopy];
    }
    return self;
}

// Implementación del método que agrega una persona a la lista.
- (void)agregarPersona:(Persona *)persona {
    [self.personas addObject:persona];
}

// Implementación del método que elimina una persona de la lista.
- (void)eliminarPersona:(Persona *)persona {
    [self.personas removeObject:persona];
}

// Implementación del método que devuelve una descripción de la lista de personas.
- (NSString *)descripcion {
    NSMutableString *descripcion = [NSMutableString string];
    [descripcion appendString:@"Lista de Personas:\n"];
    for (Persona *persona in self.personas) {
        [descripcion appendFormat:@"\t%@\n", persona.descripcion];
    }
    return descripcion;
}

@end

// Define una clase llamada "Programa" que es el punto de entrada del programa.
@interface Programa : NSObject

// Método principal del programa.
+ (void)main;

@end

// Implementación de la clase "Programa".
@implementation Programa

// Implementación del método principal del programa.
+ (void)main {
    // Crea una lista de personas.
    ListaDePersonas *listaDePersonas = [[ListaDePersonas alloc] initWithPersonas:@[
        [[Persona alloc] initWithNombre:@"Juan" edad:20],
        [[Persona alloc] initWithNombre:@"María" edad:25],
        [[Persona alloc] initWithNombre:@"Pedro" edad:30]
    ]];

    // Agrega una persona a la lista.
    [listaDePersonas agregarPersona:[[Persona alloc] initWithNombre:@"Ana" edad:35]];

    // Elimina una persona de la lista.
    [listaDePersonas eliminarPersona:[[Persona alloc] initWithNombre:@"Pedro" edad:30]];

    // Imprime la lista de personas.
    NSLog(@"%@", listaDePersonas.descripcion);
}

@end

```

Explicación del código:

* La clase `Persona` representa una persona con un nombre y una edad.
* La clase `ListaDePersonas` representa una colección de personas.
* La clase `Programa` es el punto de entrada del programa y contiene el método principal (`main`).
* En el método `main`, se crea una lista de personas, se agrega una persona a la lista, se elimina una persona de la lista y finalmente se imprime la lista de personas.

El código utiliza varias características del lenguaje Objective-C, incluyendo clases, herencia, propiedades, métodos, arrays y mutabilidad. También utiliza la biblioteca de Cocoa para imprimir la lista de personas en la consola.