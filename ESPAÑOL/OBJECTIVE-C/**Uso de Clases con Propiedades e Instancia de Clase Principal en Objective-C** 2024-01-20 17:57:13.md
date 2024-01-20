```objective-c
// Declaración de las clases e interfaces

@interface MiClase : NSObject {
    // Variables miembro
}

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSArray *lista;

- (instancetype)initWithNombre:(NSString *)nombre;
- (void)agregarElementoALista:(id)elemento;
- (NSString *)obtenerNombre;
- (NSArray *)obtenerLista;

@end


@implementation MiClase

// Implementación de los métodos

- (instancetype)initWithNombre:(NSString *)nombre {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.lista = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarElementoALista:(id)elemento {
    [self.lista addObject:elemento];
}

- (NSString *)obtenerNombre {
    return self.nombre;
}

- (NSArray *)obtenerLista {
    return self.lista;
}

@end


// Declaración de la clase principal

@interface MiClasePrincipal : NSObject {
    // Variables miembro
}

@property (nonatomic, strong) MiClase *miClase;

- (instancetype)initWithMiClase:(MiClase *)miClase;
- (void)usarMiClase;

@end


@implementation MiClasePrincipal

// Implementación de los métodos

- (instancetype)initWithMiClase:(MiClase *)miClase {
    self = [super init];
    if (self) {
        self.miClase = miClase;
    }
    return self;
}

- (void)usarMiClase {
    NSLog(@"El nombre de la clase es: %@", [self.miClase obtenerNombre]);
    NSLog(@"La lista de la clase contiene: %@", [self.miClase obtenerLista]);
}

@end


// Punto de entrada de la aplicación

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Crear una instancia de MiClase
        MiClase *miClase = [[MiClase alloc] initWithNombre:@"MiClase"];
        
        // Agregar elementos a la lista de MiClase
        [miClase agregarElementoALista:@"Uno"];
        [miClase agregarElementoALista:@"Dos"];
        [miClase agregarElementoALista:@"Tres"];
        
        // Crear una instancia de MiClasePrincipal
        MiClasePrincipal *miClasePrincipal = [[MiClasePrincipal alloc] initWithMiClase:miClase];
        
        // Usar MiClase desde MiClasePrincipal
        [miClasePrincipal usarMiClase];
    }
    return 0;
}
```

Este código crea una clase llamada `MiClase` con dos propiedades, `nombre` y `lista`. La propiedad `nombre` es una cadena de caracteres y la propiedad `lista` es un arreglo de objetos. La clase `MiClase` también tiene cuatro métodos:

* `initWithNombre:`: Este método inicializador crea una nueva instancia de `MiClase` y le asigna un nombre.
* `agregarElementoALista:`: Este método agrega un elemento a la lista de `MiClase`.
* `obtenerNombre`: Este método devuelve el nombre de `MiClase`.
* `obtenerLista`: Este método devuelve la lista de `MiClase`.

El código también crea una clase principal llamada `MiClasePrincipal`. Esta clase tiene una propiedad llamada `miClase`, que es una instancia de `MiClase`. La clase `MiClasePrincipal` también tiene dos métodos:

* `initWithMiClase:`: Este método inicializador crea una nueva instancia de `MiClasePrincipal` y le asigna una instancia de `MiClase`.
* `usarMiClase`: Este método usa la instancia de `MiClase` para imprimir el nombre y la lista de `MiClase`.

El punto de entrada de la aplicación está en la función `main`. Esta función crea una instancia de `MiClase` y agrega algunos elementos a su lista. Luego crea una instancia de `MiClasePrincipal` y usa la instancia de `MiClase` para imprimir el nombre y la lista de `MiClase`.