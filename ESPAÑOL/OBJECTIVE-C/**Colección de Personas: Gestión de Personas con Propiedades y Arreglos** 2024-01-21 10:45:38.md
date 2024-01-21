```objective-c
// Crear una clase llamada "Persona" con propiedades para el nombre, la edad y el género.

@interface Persona : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, assign) NSInteger edad;
@property (nonatomic, assign) BOOL esHombre;

@end

// Implementar los métodos de la clase "Persona".

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad esHombre:(BOOL)esHombre {
    if (self = [super init]) {
        self.nombre = nombre;
        self.edad = edad;
        self.esHombre = esHombre;
    }
    return self;
}

- (NSString *)description {
    return [NSString stringWithFormat:@"Nombre: %@, Edad: %ld, Es Hombre: %@", self.nombre, self.edad, self.esHombre ? @"Sí" : @"No"];
}

@end

// Crear una clase llamada "ColeccionDePersonas" con propiedades para almacenar un arreglo de objetos de la clase "Persona".

@interface ColeccionDePersonas : NSObject

@property (nonatomic, strong) NSMutableArray *personas;

@end

// Implementar los métodos de la clase "ColeccionDePersonas".

@implementation ColeccionDePersonas

- (instancetype)init {
    if (self = [super init]) {
        self.personas = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarPersona:(Persona *)persona {
    [self.personas addObject:persona];
}

- (NSArray *)obtenerPersonas {
    return [self.personas copy];
}

@end

// Crear una instancia de la clase "ColeccionDePersonas" y agregar algunas personas a la misma.

ColeccionDePersonas *coleccionDePersonas = [[ColeccionDePersonas alloc] init];

Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20 esHombre:YES];
Persona *persona2 = [[Persona alloc] initWithNombre:@"María" edad:25 esHombre:NO];
Persona *persona3 = [[Persona alloc] initWithNombre:@"Pedro" edad:30 esHombre:YES];

[coleccionDePersonas agregarPersona:persona1];
[coleccionDePersonas agregarPersona:persona2];
[coleccionDePersonas agregarPersona:persona3];

// Imprimir los datos de las personas en la colección.

NSArray *personas = [coleccionDePersonas obtenerPersonas];

for (Persona *persona in personas) {
    NSLog(@"%@", persona);
}

```

Explicación del código:

1. Se crea una clase llamada "Persona" con propiedades para el nombre, la edad y el género.
2. Se implementa los métodos de la clase "Persona", incluyendo un método inicializador y un método para obtener una descripción de la persona.
3. Se crea una clase llamada "ColeccionDePersonas" con una propiedad para almacenar un arreglo de objetos de la clase "Persona".
4. Se implementa los métodos de la clase "ColeccionDePersonas", incluyendo un método para agregar personas a la colección y un método para obtener un arreglo con todas las personas en la colección.
5. Se crea una instancia de la clase "ColeccionDePersonas" y se agregan algunas personas a la misma.
6. Se imprime los datos de las personas en la colección utilizando el método `NSLog()`.