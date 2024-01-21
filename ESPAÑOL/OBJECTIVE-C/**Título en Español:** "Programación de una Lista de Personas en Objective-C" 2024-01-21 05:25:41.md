```objective-c
#import <Foundation/Foundation.h>

// Definir una clase llamada "Persona"
@interface Persona : NSObject

// Propiedades de la clase "Persona"
@property NSString *nombre;
@property int edad;

// Método inicializador de la clase "Persona"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad;

// Método para obtener el nombre de la persona
- (NSString *)getNombre;

// Método para obtener la edad de la persona
- (int)getEdad;

// Método para establecer el nombre de la persona
- (void)setNombre:(NSString *)nombre;

// Método para establecer la edad de la persona
- (void)setEdad:(int)edad;

// Método para imprimir los datos de la persona
- (void)imprimirDatos;

@end

// Implementar la clase "Persona"
@implementation Persona

// Método inicializador de la clase "Persona"
- (instancetype)initWithNombre:(NSString *)nombre edad:(int)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.edad = edad;
    }
    return self;
}

// Método para obtener el nombre de la persona
- (NSString *)getNombre {
    return self.nombre;
}

// Método para obtener la edad de la persona
- (int)getEdad {
    return self.edad;
}

// Método para establecer el nombre de la persona
- (void)setNombre:(NSString *)nombre {
    self.nombre = nombre;
}

// Método para establecer la edad de la persona
- (void)setEdad:(int)edad {
    self.edad = edad;
}

// Método para imprimir los datos de la persona
- (void)imprimirDatos {
    NSLog(@"Nombre: %@", self.nombre);
    NSLog(@"Edad: %d", self.edad);
}

@end

// Definir una clase llamada "ListaPersonas"
@interface ListaPersonas : NSObject

// Propiedad de la clase "ListaPersonas"
@property NSMutableArray *personas;

// Método inicializador de la clase "ListaPersonas"
- (instancetype)init;

// Método para agregar una persona a la lista
- (void)agregarPersona:(Persona *)persona;

// Método para eliminar una persona de la lista
- (void)eliminarPersona:(Persona *)persona;

// Método para obtener el número de personas en la lista
- (int)getNumeroPersonas;

// Método para obtener la persona en una posición específica de la lista
- (Persona *)getPersonaEnPosicion:(int)posicion;

// Método para imprimir los datos de todas las personas en la lista
- (void)imprimirDatosTodasPersonas;

@end

// Implementar la clase "ListaPersonas"
@implementation ListaPersonas

// Método inicializador de la clase "ListaPersonas"
- (instancetype)init {
    self = [super init];
    if (self) {
        self.personas = [[NSMutableArray alloc] init];
    }
    return self;
}

// Método para agregar una persona a la lista
- (void)agregarPersona:(Persona *)persona {
    [self.personas addObject:persona];
}

// Método para eliminar una persona de la lista
- (void)eliminarPersona:(Persona *)persona {
    [self.personas removeObject:persona];
}

// Método para obtener el número de personas en la lista
- (int)getNumeroPersonas {
    return (int)[self.personas count];
}

// Método para obtener la persona en una posición específica de la lista
- (Persona *)getPersonaEnPosicion:(int)posicion {
    return [self.personas objectAtIndex:posicion];
}

// Método para imprimir los datos de todas las personas en la lista
- (void)imprimirDatosTodasPersonas {
    for (Persona *persona in self.personas) {
        [persona imprimirDatos];
    }
}

@end

// Función principal del programa
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una lista de personas
        ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];

        // Agregar algunas personas a la lista
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20];
        Persona *persona2 = [[Persona alloc] initWithNombre:@"Maria" edad:25];
        Persona *persona3 = [[Persona alloc] initWithNombre:@"Pedro" edad:30];
        [listaPersonas agregarPersona:persona1];
        [listaPersonas agregarPersona:persona2];
        [listaPersonas agregarPersona:persona3];

        // Imprimir los datos de todas las personas en la lista
        NSLog(@"Lista de personas:");
        [listaPersonas imprimirDatosTodasPersonas];

        // Obtener la persona en la posición 1 de la lista
        Persona *personaEnPosicion1 = [listaPersonas getPersonaEnPosicion:1];

        // Imprimir los datos de la persona en la posición 1 de la lista
        NSLog(@"Persona en la posición 1:");
        [personaEnPosicion1 imprimirDatos];

        // Eliminar la persona en la posición 2 de la lista
        [listaPersonas eliminarPersona:persona2];

        // Imprimir los datos de todas las personas en la lista
        NSLog(@"Lista de personas después de eliminar a Maria:");
        [listaPersonas imprimirDatosTodasPersonas];
    }
    return 0;
}
```

Explicación del código:

* La primera parte del código define la clase `Persona`, que representa a una persona con un nombre y una edad.
* La segunda parte del código define la clase `ListaPersonas`, que es una lista de personas.
* La tercera parte del código define la función principal del programa, que crea una lista de personas, agrega algunas personas a la lista, imprime los datos de todas las personas en la lista, obtiene la persona en una posición específica de la lista, elimina una persona de la lista e imprime los datos de todas las personas en la lista después de eliminar a la persona.