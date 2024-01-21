```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        _nombre = nombre;
        _apellido = apellido;
        _edad = edad;
    }
    return self;
}

@end

@interface ListaPersonas : NSObject

@property (nonatomic, strong) NSMutableArray<Persona *> *personas;

- (void)agregarPersona:(Persona *)persona;
- (void)eliminarPersona:(Persona *)persona;
- (NSArray<Persona *> *)obtenerPersonas;

@end

@implementation ListaPersonas

- (instancetype)init {
    self = [super init];
    if (self) {
        _personas = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarPersona:(Persona *)persona {
    [_personas addObject:persona];
}

- (void)eliminarPersona:(Persona *)persona {
    [_personas removeObject:persona];
}

- (NSArray<Persona *> *)obtenerPersonas {
    return _personas;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una lista de personas
        ListaPersonas *listaPersonas = [[ListaPersonas alloc] init];
        
        // Agregar algunas personas a la lista
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Juan" apellido:@"García" edad:25]];
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"María" apellido:@"Pérez" edad:30]];
        [listaPersonas agregarPersona:[[Persona alloc] initWithNombre:@"Pedro" apellido:@"López" edad:35]];
        
        // Obtener las personas de la lista
        NSArray<Persona *> *personas = [listaPersonas obtenerPersonas];
        
        // Imprimir las personas en la consola
        for (Persona *persona in personas) {
            NSLog(@"Nombre: %@ %@", persona.nombre, persona.apellido);
            NSLog(@"Edad: %ld", persona.edad);
            NSLog(@"--------------------------------");
        }
    }
    return 0;
}
```

**Explicación del código:**

1. Definir la clase `Persona`.

La clase `Persona` representa a una persona individual. Tiene tres propiedades: `nombre`, `apellido` y `edad`.

2. Definir la clase `ListaPersonas`.

La clase `ListaPersonas` representa una lista de personas. Tiene una propiedad: `personas`, que es un array mutable de objetos `Persona`.

3. Crear una lista de personas.

En la función `main`, se crea una lista de personas utilizando la clase `ListaPersonas`.

4. Agregar algunas personas a la lista.

Se agregan tres personas a la lista utilizando el método `agregarPersona` de la clase `ListaPersonas`.

5. Obtener las personas de la lista.

Se obtienen las personas de la lista utilizando el método `obtenerPersonas` de la clase `ListaPersonas`.

6. Imprimir las personas en la consola.

Se utiliza el bucle `for` para imprimir las personas en la consola.

Este código es complejo porque tiene múltiples clases, propiedades, métodos y bucles. Sin embargo, es un código bien estructurado y fácil de entender.