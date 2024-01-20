```objective-c
#import <Foundation/Foundation.h>

// Clase para representar una persona
@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
}

// Método para inicializar una persona
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;

// Métodos para obtener el nombre y la edad
- (NSString *)nombre;
- (int)edad;

@end

// Implementación de la clase Persona
@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad
{
    self = [super init];
    if (self) {
        self->nombre = nombre;
        self->edad = edad;
    }
    return self;
}

- (NSString *)nombre
{
    return nombre;
}

- (int)edad
{
    return edad;
}

@end

// Clase para representar una lista de personas
@interface ListaPersonas : NSObject
{
    NSMutableArray *personas;
}

// Método para inicializar una lista de personas
- (instancetype)init;

// Método para añadir una persona a la lista
- (void)addPersona:(Persona *)persona;

// Método para obtener la lista de personas
- (NSArray *)personas;

@end

// Implementación de la clase ListaPersonas
@implementation ListaPersonas

- (instancetype)init
{
    self = [super init];
    if (self) {
        personas = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)addPersona:(Persona *)persona
{
    [personas addObject:persona];
}

- (NSArray *)personas
{
    return personas;
}

@end

// Función principal
int main(int argc, const char * argv[])
{
    @autoreleasepool {
        // Creamos una lista de personas
        ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];

        // Añadimos algunas personas a la lista
        [listaPersonas addPersona:[[Persona alloc] initWithNombre:@"Juan" edad:20]];
        [listaPersonas addPersona:[[Persona alloc] initWithNombre:@"María" edad:25]];
        [listaPersonas addPersona:[[Persona alloc] initWithNombre:@"Pedro" edad:30]];

        // Obtenemos la lista de personas
        NSArray *personas = [listaPersonas personas];

        // Imprimimos la lista de personas
        for (Persona *persona in personas) {
            NSLog(@"Nombre: %@, Edad: %d", [persona nombre], [persona edad]);
        }
    }
    return 0;
}
```

Explicación:

* La clase `Persona` representa a una persona con un nombre y una edad.
* La clase `ListaPersonas` representa una lista de personas.
* La función `main` crea una lista de personas, añade algunas personas a la lista y luego imprime la lista.

Este código es complejo porque utiliza varias clases, métodos y propiedades. También utiliza la biblioteca Foundation, que es una biblioteca estándar de Objective-C que proporciona muchas funciones útiles.

Este código también es complejo porque utiliza la programación orientada a objetos, que es un paradigma de programación que permite crear objetos que representan entidades del mundo real. Esto hace que sea más fácil organizar y mantener el código.