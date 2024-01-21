```objective-c
// Definición de una clase llamada "Persona" con propiedades "nombre" y "edad".
@interface Persona : NSObject
@property(nonatomic, copy) NSString *nombre;
@property(nonatomic) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad;

- (NSString *)descripcion;
@end

// Implementación de la clase "Persona".
@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad {
  if (self = [super init]) {
    self.nombre = nombre;
    self.edad = edad;
  }
  return self;
}

- (NSString *)descripcion {
  NSMutableString *descripcion = [NSMutableString stringWithString:[NSString stringWithFormat:@"Nombre: %@", self.nombre]];
  [descripcion appendFormat:@", Edad: %ld", self.edad];
  return [descripcion copy];
}

@end

// Definición de una clase llamada "ListaDePersonas" que almacena una lista de objetos "Persona".
@interface ListaDePersonas : NSObject

// Una propiedad que almacena un array de objetos "Persona".
@property(nonatomic, copy) NSArray *personas;

// Un método para agregar una persona a la lista.
- (void)agregarPersona:(Persona *)persona;

// Un método para eliminar una persona de la lista.
- (void)eliminarPersona:(Persona *)persona;

// Un método para obtener la lista de personas.
- (NSArray *)obtenerPersonas;

@end

// Implementación de la clase "ListaDePersonas".
@implementation ListaDePersonas

- (instancetype)init {
  if (self = [super init]) {
    self.personas = [[NSMutableArray alloc] init];
  }
  return self;
}

- (void)agregarPersona:(Persona *)persona {
  [(NSMutableArray *)self.personas addObject:persona];
}

- (void)eliminarPersona:(Persona *)persona {
  [(NSMutableArray *)self.personas removeObject:persona];
}

- (NSArray *)obtenerPersonas {
  return [self.personas copy];
}

@end

// Definición de una clase llamada "GestorDePersonas" que administra una lista de personas.
@interface GestorDePersonas : NSObject

// Una propiedad que almacena una instancia de "ListaDePersonas".
@property(nonatomic, strong) ListaDePersonas *listaDePersonas;

// Método para inicializar el gestor de personas con una lista de personas.
- (instancetype)initWithListaDePersonas:(ListaDePersonas *)listaDePersonas;

// Método para obtener la lista de personas.
- (ListaDePersonas *)obtenerListaDePersonas;

// Método para añadir una persona a la lista.
- (void)agregarPersona:(Persona *)persona;

// Método para eliminar una persona de la lista.
- (void)eliminarPersona:(Persona *)persona;

// Método para obtener una persona por su nombre.
- (Persona *)obtenerPersonaPorNombre:(NSString *)nombre;

@end

// Implementación de la clase "GestorDePersonas".
@implementation GestorDePersonas

- (instancetype)initWithListaDePersonas:(ListaDePersonas *)listaDePersonas {
  if (self = [super init]) {
    self.listaDePersonas = listaDePersonas;
  }
  return self;
}

- (ListaDePersonas *)obtenerListaDePersonas {
  return self.listaDePersonas;
}

- (void)agregarPersona:(Persona *)persona {
  [self.listaDePersonas agregarPersona:persona];
}

- (void)eliminarPersona:(Persona *)persona {
  [self.listaDePersonas eliminarPersona:persona];
}

- (Persona *)obtenerPersonaPorNombre:(NSString *)nombre {
  NSArray *personas = [self.listaDePersonas obtenerPersonas];
  for (Persona *persona in personas) {
    if ([persona.nombre isEqualToString:nombre]) {
      return persona;
    }
  }
  return nil;
}

@end

int main(int argc, const char *argv[]) {
  @autoreleasepool {
    // Crear una lista de personas.
    ListaDePersonas *listaDePersonas = [[ListaDePersonas alloc] init];

    // Crear algunas personas y añadirlas a la lista.
    Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:25];
    Persona *persona2 = [[Persona alloc] initWithNombre:@"María" edad:30];
    Persona *persona3 = [[Persona alloc] initWithNombre:@"Pedro" edad:35];
    [listaDePersonas agregarPersona:persona1];
    [listaDePersonas agregarPersona:persona2];
    [listaDePersonas agregarPersona:persona3];

    // Crear un gestor de personas y pasarle la lista de personas.
    GestorDePersonas *gestorDePersonas = [[GestorDePersonas alloc] initWithListaDePersonas:listaDePersonas];

    // Obtener una lista de todas las personas.
    NSArray *personas = [gestorDePersonas obtenerListaDePersonas];

    // Imprimir la lista de personas.
    for (Persona *persona in personas) {
      NSLog(@"%@", persona.descripcion);
    }

    // Obtener una persona por su nombre.
    Persona *personaPorNombre = [gestorDePersonas obtenerPersonaPorNombre:@"María"];

    // Imprimir la persona obtenida.
    NSLog(@"%@", personaPorNombre.descripcion);
  }
  return 0;
}
```

Explicación:

* Creamos una clase llamada "Persona" que almacena el nombre y la edad de una persona.
* También creamos una clase llamada "ListaDePersonas" que almacena una lista de objetos "Persona".
* Definimos una clase llamada "GestorDePersonas" que administra una lista de personas y proporciona métodos para agregar, eliminar y obtener personas de la lista.
* En la función `main`, creamos una lista de personas, añadimos algunas personas a la lista y luego creamos un gestor de personas y le pasamos la lista de personas.
* Luego obtenemos una lista de todas las personas y la imprimimos.
* También obtenemos una persona por su nombre e imprimimos su descripción.

Este código demuestra el uso de clases, objetos, propiedades, métodos y arrays en Objective-C.