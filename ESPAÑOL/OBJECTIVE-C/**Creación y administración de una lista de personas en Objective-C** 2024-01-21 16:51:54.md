```objective-c
#import <Foundation/Foundation.h>

// Definir una clase Persona con propiedades para nombre, edad y género
@interface Persona : NSObject
{
    NSString *nombre;
    int edad;
    NSString *genero;
}

- (id)initWithNombre:(NSString *)nombre edad:(int)edad genero:(NSString *)genero;
- (NSString *)nombre;
- (void)setNombre:(NSString *)nombre;
- (int)edad;
- (void)setEdad:(int)edad;
- (NSString *)genero;
- (void)setGenero:(NSString *)genero;
- (NSString *)descripcion;

@end

// Implementación de la clase Persona
@implementation Persona

- (id)initWithNombre:(NSString *)nombre edad:(int)edad genero:(NSString *)genero
{
    self = [super init];
    if (self) {
        self->nombre = nombre;
        self->edad = edad;
        self->genero = genero;
    }
    return self;
}

- (NSString *)nombre
{
    return nombre;
}

- (void)setNombre:(NSString *)nombre
{
    self->nombre = nombre;
}

- (int)edad
{
    return edad;
}

- (void)setEdad:(int)edad
{
    self->edad = edad;
}

- (NSString *)genero
{
    return genero;
}

- (void)setGenero:(NSString *)genero
{
    self->genero = genero;
}

- (NSString *)descripcion
{
    return [NSString stringWithFormat:@"Nombre: %@, Edad: %d, Genero: %@", nombre, edad, genero];
}

@end

// Definir una clase ListaPersonas para almacenar una lista de objetos Persona
@interface ListaPersonas : NSObject
{
    NSMutableArray *lista;
}

- (id)init;
- (void)agregarPersona:(Persona *)persona;
- (Persona *)obtenerPersonaPorIndice:(int)indice;
- (int)contarPersonas;
- (NSString *)descripcion;

@end

// Implementación de la clase ListaPersonas
@implementation ListaPersonas

- (id)init
{
    self = [super init];
    if (self) {
        lista = [NSMutableArray array];
    }
    return self;
}

- (void)agregarPersona:(Persona *)persona
{
    [lista addObject:persona];
}

- (Persona *)obtenerPersonaPorIndice:(int)indice
{
    return [lista objectAtIndex:indice];
}

- (int)contarPersonas
{
    return [lista count];
}

- (NSString *)descripcion
{
    NSMutableString *descripcion = [NSMutableString string];
    for (Persona *persona in lista) {
        [descripcion appendString:[persona descripcion]];
        [descripcion appendString:@"\n"];
    }
    return descripcion;
}

@end

// Crear una instancia de la clase ListaPersonas
ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];

// Agregar algunas personas a la lista
[listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Juan" edad:20 genero:@"Masculino"]];
[listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"María" edad:25 genero:@"Femenino"]];
[listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Pedro" edad:30 genero:@"Masculino"]];

// Obtener una persona de la lista por índice
Persona *personaObtenida = [listaPersonas obtenerPersonaPorIndice:1];

// Imprimir la descripción de la lista de personas y de la persona obtenida
NSLog(@"%@", [listaPersonas descripcion]);
NSLog(@"%@", [personaObtenida descripcion]);
```

Explicación del código:

* Se definen dos clases: `Persona` y `ListaPersonas`.
* La clase `Persona` representa a una persona con propiedades para su nombre, edad y género.
* La clase `ListaPersonas` representa una lista de objetos `Persona`.
* Se crea una instancia de la clase `ListaPersonas` y se agregan algunas personas a la lista.
* Se obtiene una persona de la lista por índice y se imprime la descripción de la lista de personas y de la persona obtenida.

Este código es complejo porque utiliza múltiples clases, propiedades y métodos, y realiza varias operaciones, como crear objetos, agregar objetos a una lista y obtener un objeto de una lista.