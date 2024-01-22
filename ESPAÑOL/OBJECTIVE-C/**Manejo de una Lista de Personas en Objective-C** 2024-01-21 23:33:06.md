```objective-c
#import <Foundation/Foundation.h>

// Definición de la clase `Persona`
@interface Persona : NSObject

// Propiedades de la clase `Persona`
@property NSString *nombre;
@property NSInteger edad;

// Método inicializador de la clase `Persona`
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad;

// Método que devuelve una descripción de la persona
- (NSString *)descripcion;

@end

// Implementación de la clase `Persona`
@implementation Persona

// Método inicializador de la clase `Persona`
- (instancetype)initWithNombre:(NSString *)nombre edad:(NSInteger)edad {
  if (self = [super init]) {
    self.nombre = nombre;
    self.edad = edad;
  }
  return self;
}

// Método que devuelve una descripción de la persona
- (NSString *)descripcion {
  return [NSString stringWithFormat:@"Nombre: %@, Edad: %ld", self.nombre, self.edad];
}

@end

// Definición de la clase `ListaPersonas`
@interface ListaPersonas : NSObject

// Propiedades de la clase `ListaPersonas`
@property NSMutableArray<Persona *> *listaPersonas;

// Método inicializador de la clase `ListaPersonas`
- (instancetype)init;

// Método que añade una persona a la lista
- (void)agregarPersona:(Persona *)persona;

// Método que elimina una persona de la lista
- (void)eliminarPersona:(Persona *)persona;

// Método que devuelve una lista de las personas en la lista
- (NSArray<Persona *> *)obtenerListaPersonas;

@end

// Implementación de la clase `ListaPersonas`
@implementation ListaPersonas

// Método inicializador de la clase `ListaPersonas`
- (instancetype)init {
  if (self = [super init]) {
    self.listaPersonas = [[NSMutableArray alloc] init];
  }
  return self;
}

// Método que añade una persona a la lista
- (void)agregarPersona:(Persona *)persona {
  [self.listaPersonas addObject:persona];
}

// Método que elimina una persona de la lista
- (void)eliminarPersona:(Persona *)persona {
  [self.listaPersonas removeObject:persona];
}

// Método que devuelve una lista de las personas en la lista
- (NSArray<Persona *> *)obtenerListaPersonas {
  return [self.listaPersonas copy];
}

@end

// Definición de la clase `Main`
@interface Main : NSObject

// Método principal del programa
+ (void)main;

@end

// Implementación de la clase `Main`
@implementation Main

// Método principal del programa
+ (void)main {
  // Crear una lista de personas
  ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];

  // Añadir personas a la lista
  Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" edad:20];
  Persona *persona2 = [[Persona alloc] initWithNombre:@"María" edad:25];
  Persona *persona3 = [[Persona alloc] initWithNombre:@"Pedro" edad:30];
  [listaPersonas agregarPersona:persona1];
  [listaPersonas agregarPersona:persona2];
  [listaPersonas agregarPersona:persona3];

  // Obtener la lista de personas
  NSArray<Persona *> *listaPersonasObtenida = [listaPersonas obtenerListaPersonas];

  // Imprimir la lista de personas
  for (Persona *persona in listaPersonasObtenida) {
    NSLog(@"%@", persona.descripcion);
  }

  // Eliminar una persona de la lista
  [listaPersonas eliminarPersona:persona1];

  // Obtener la lista de personas después de eliminar una persona
  listaPersonasObtenida = [listaPersonas obtenerListaPersonas];

  // Imprimir la lista de personas después de eliminar una persona
  NSLog(@"Lista de personas después de eliminar a Juan:");
  for (Persona *persona in listaPersonasObtenida) {
    NSLog(@"%@", persona.descripcion);
  }
}

@end

// Punto de entrada al programa
int main(int argc, const char *argv[]) {
  @autoreleasepool {
    // Iniciar el programa
    [Main main];
  }
  return 0;
}
```

**Explicación del código:**

* La clase `Persona` representa a una persona con las propiedades `nombre` y `edad`.
* La clase `ListaPersonas` representa una lista de personas con las propiedades `listaPersonas` que es un `NSMutableArray` de objetos `Persona`.
* La clase `Main` contiene el método `main` que es el punto de entrada al programa.
* En el método `main` se crea una lista de personas y se añaden tres personas a la lista.
* Luego, se obtiene una lista de las personas en la lista y se imprime en la consola.
* A continuación, se elimina una persona de la lista y se obtiene la lista de personas después de eliminar una persona.
* Finalmente, se imprime la lista de personas después de eliminar una persona en la consola.